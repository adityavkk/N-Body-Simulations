import type { Body } from '../engine/types';

let nextId = 0;
export function resetIdCounter(): void {
  nextId = 0;
}

export function makeBody(
  mass: number,
  x: number, y: number, z: number,
  vx: number, vy: number, vz: number,
  radius: number,
  color: [number, number, number],
  fixed = false
): Body {
  return {
    id: nextId++,
    mass, x, y, z, vx, vy, vz, radius, color, trail: [], fixed,
  };
}

/** Generate a random number from a Gaussian distribution */
export function gaussRandom(mean = 0, stdev = 1): number {
  const u = 1 - Math.random();
  const v = Math.random();
  const z = Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
  return z * stdev + mean;
}

/** Generate random point on a sphere surface */
export function randomOnSphere(radius: number): [number, number, number] {
  const theta = Math.random() * Math.PI * 2;
  const phi = Math.acos(2 * Math.random() - 1);
  return [
    radius * Math.sin(phi) * Math.cos(theta),
    radius * Math.sin(phi) * Math.sin(theta),
    radius * Math.cos(phi),
  ];
}

/** Generate random point in a disk */
export function randomInDisk(innerRadius: number, outerRadius: number, thickness: number): [number, number, number] {
  const r = innerRadius + Math.sqrt(Math.random()) * (outerRadius - innerRadius);
  const theta = Math.random() * Math.PI * 2;
  return [
    r * Math.cos(theta),
    gaussRandom(0, thickness),
    r * Math.sin(theta),
  ];
}

/** Calculate circular orbit velocity for a body at distance r from central mass M */
export function circularVelocity(G: number, M: number, r: number): number {
  return Math.sqrt(G * M / r);
}
