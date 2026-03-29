import type { Body } from './types';

interface OctreeNode {
  // Bounding box center + half-width
  cx: number;
  cy: number;
  cz: number;
  halfWidth: number;

  // Center of mass
  comX: number;
  comY: number;
  comZ: number;
  totalMass: number;
  bodyCount: number;

  // If leaf, store the body
  body: Body | null;

  // 8 children (octants)
  children: (OctreeNode | null)[];
}

function createNode(cx: number, cy: number, cz: number, halfWidth: number): OctreeNode {
  return {
    cx, cy, cz, halfWidth,
    comX: 0, comY: 0, comZ: 0,
    totalMass: 0, bodyCount: 0,
    body: null,
    children: [null, null, null, null, null, null, null, null],
  };
}

function getOctant(node: OctreeNode, x: number, y: number, z: number): number {
  let octant = 0;
  if (x > node.cx) octant |= 1;
  if (y > node.cy) octant |= 2;
  if (z > node.cz) octant |= 4;
  return octant;
}

function childCenter(node: OctreeNode, octant: number): [number, number, number] {
  const q = node.halfWidth * 0.5;
  return [
    node.cx + ((octant & 1) ? q : -q),
    node.cy + ((octant & 2) ? q : -q),
    node.cz + ((octant & 4) ? q : -q),
  ];
}

function insert(node: OctreeNode, body: Body): void {
  if (node.bodyCount === 0) {
    // Empty leaf — place body here
    node.body = body;
    node.comX = body.x;
    node.comY = body.y;
    node.comZ = body.z;
    node.totalMass = body.mass;
    node.bodyCount = 1;
    return;
  }

  // If this is a leaf with one body, push existing body down
  if (node.body !== null) {
    const existing = node.body;
    node.body = null;
    const oct = getOctant(node, existing.x, existing.y, existing.z);
    if (node.children[oct] === null) {
      const [cx, cy, cz] = childCenter(node, oct);
      node.children[oct] = createNode(cx, cy, cz, node.halfWidth * 0.5);
    }
    insert(node.children[oct]!, existing);
  }

  // Insert new body into appropriate child
  const oct = getOctant(node, body.x, body.y, body.z);
  if (node.children[oct] === null) {
    const [cx, cy, cz] = childCenter(node, oct);
    node.children[oct] = createNode(cx, cy, cz, node.halfWidth * 0.5);
  }
  insert(node.children[oct]!, body);

  // Update center of mass
  const newMass = node.totalMass + body.mass;
  node.comX = (node.comX * node.totalMass + body.x * body.mass) / newMass;
  node.comY = (node.comY * node.totalMass + body.y * body.mass) / newMass;
  node.comZ = (node.comZ * node.totalMass + body.z * body.mass) / newMass;
  node.totalMass = newMass;
  node.bodyCount++;
}

/** Build an octree from a list of bodies */
export function buildOctree(bodies: Body[]): OctreeNode | null {
  if (bodies.length === 0) return null;

  // Find bounding box
  let minX = Infinity, minY = Infinity, minZ = Infinity;
  let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;
  for (const b of bodies) {
    if (b.x < minX) minX = b.x;
    if (b.y < minY) minY = b.y;
    if (b.z < minZ) minZ = b.z;
    if (b.x > maxX) maxX = b.x;
    if (b.y > maxY) maxY = b.y;
    if (b.z > maxZ) maxZ = b.z;
  }

  const cx = (minX + maxX) * 0.5;
  const cy = (minY + maxY) * 0.5;
  const cz = (minZ + maxZ) * 0.5;
  const halfWidth = Math.max(maxX - minX, maxY - minY, maxZ - minZ) * 0.5 + 1;

  const root = createNode(cx, cy, cz, halfWidth);
  for (const b of bodies) {
    insert(root, b);
  }
  return root;
}

/** Calculate acceleration on a body using Barnes-Hut approximation */
export function calcAcceleration(
  body: Body,
  root: OctreeNode,
  G: number,
  theta: number,
  softening: number
): [number, number, number] {
  let ax = 0, ay = 0, az = 0;

  const stack: OctreeNode[] = [root];
  while (stack.length > 0) {
    const node = stack.pop()!;
    if (node.bodyCount === 0) continue;

    const dx = node.comX - body.x;
    const dy = node.comY - body.y;
    const dz = node.comZ - body.z;
    const distSq = dx * dx + dy * dy + dz * dz + softening * softening;
    const dist = Math.sqrt(distSq);

    // If leaf with single body (and it's not this body)
    if (node.bodyCount === 1 && node.body !== null) {
      if (node.body.id !== body.id) {
        const force = G * node.totalMass / (distSq * dist);
        ax += dx * force;
        ay += dy * force;
        az += dz * force;
      }
      continue;
    }

    // Barnes-Hut criterion: if node is far enough, treat as point mass
    const width = node.halfWidth * 2;
    if (width / dist < theta) {
      const force = G * node.totalMass / (distSq * dist);
      ax += dx * force;
      ay += dy * force;
      az += dz * force;
    } else {
      // Recurse into children
      for (let i = 0; i < 8; i++) {
        if (node.children[i] !== null) {
          stack.push(node.children[i]!);
        }
      }
    }
  }

  return [ax, ay, az];
}
