import type { SimulationConfig } from '../engine/types';
import { resetIdCounter, makeBody, gaussRandom, randomInDisk, circularVelocity, randomOnSphere } from './helpers';

// ── Solar System ──────────────────────────────────────────────────

function solarSystem(): SimulationConfig {
  resetIdCounter();
  const G = 2.0;
  const sunMass = 50000;

  const sun = makeBody(sunMass, 0, 0, 0, 0, 0, 0, 8, [1, 0.95, 0.6], true);

  const planetData: [number, number, number, [number, number, number], string][] = [
    // mass, distance, radius, color, name
    [0.5, 60, 1.5, [0.7, 0.7, 0.7], 'Mercury'],
    [2, 90, 2.5, [1, 0.8, 0.4], 'Venus'],
    [2.5, 130, 2.8, [0.2, 0.5, 1], 'Earth'],
    [1.5, 175, 2.2, [0.9, 0.3, 0.1], 'Mars'],
    [20, 280, 5, [0.9, 0.7, 0.4], 'Jupiter'],
    [15, 400, 4.5, [0.9, 0.85, 0.5], 'Saturn'],
    [8, 540, 3.5, [0.5, 0.8, 0.9], 'Uranus'],
    [8, 680, 3.5, [0.3, 0.4, 0.9], 'Neptune'],
  ];

  const planets = planetData.map(([mass, dist, radius, color]) => {
    const v = circularVelocity(G, sunMass, dist);
    const angle = Math.random() * Math.PI * 2;
    return makeBody(
      mass,
      dist * Math.cos(angle), 0, dist * Math.sin(angle),
      -v * Math.sin(angle), 0, v * Math.cos(angle),
      radius, color as [number, number, number]
    );
  });

  return {
    name: 'Solar System',
    description: '8 planets orbiting a central star with realistic mass ratios',
    bodies: [sun, ...planets],
    gravitationalConstant: G,
    softening: 5,
    timeStep: 0.05,
    theta: 0.5,
    trailLength: 400,
    is3D: false,
    scale: 800,
    bloomStrength: 1.5,
  };
}

// ── Binary Stars ──────────────────────────────────────────────────

function binaryStars(): SimulationConfig {
  resetIdCounter();
  const G = 1.0;
  const starMass = 5000;
  const sep = 200;
  const v = Math.sqrt(G * starMass / (2 * sep));

  const star1 = makeBody(starMass, -sep / 2, 0, 0, 0, 0, v, 6, [1, 0.5, 0.2]);
  const star2 = makeBody(starMass, sep / 2, 0, 0, 0, 0, -v, 6, [0.3, 0.5, 1]);

  // Add some debris/planets around each star
  const debris: ReturnType<typeof makeBody>[] = [];
  for (let i = 0; i < 80; i++) {
    const whichStar = i < 40 ? star1 : star2;
    const dist = 30 + Math.random() * 60;
    const angle = Math.random() * Math.PI * 2;
    const orbitV = circularVelocity(G, starMass, dist);
    debris.push(makeBody(
      0.1,
      whichStar.x + dist * Math.cos(angle),
      gaussRandom(0, 3),
      whichStar.z + dist * Math.sin(angle),
      whichStar.vx - orbitV * Math.sin(angle),
      0,
      whichStar.vz + orbitV * Math.cos(angle),
      0.8,
      whichStar === star1 ? [1, 0.6, 0.3] : [0.4, 0.6, 1]
    ));
  }

  return {
    name: 'Binary Stars',
    description: 'Two stars orbiting their common center of mass with debris disks',
    bodies: [star1, star2, ...debris],
    gravitationalConstant: G,
    softening: 3,
    timeStep: 0.08,
    theta: 0.5,
    trailLength: 300,
    is3D: true,
    scale: 400,
    bloomStrength: 1.8,
  };
}

// ── Figure Eight ──────────────────────────────────────────────────

function figureEight(): SimulationConfig {
  resetIdCounter();
  const G = 1.0;
  const m = 1000;

  // Cris Moore's figure-8 solution (scaled up)
  const s = 120;
  const p1 = 0.347111;
  const p2 = 0.532728;

  return {
    name: 'Figure Eight',
    description: 'The famous three-body periodic figure-8 choreography',
    bodies: [
      makeBody(m, -s, 0, 0, G * p1, 0, G * p2, 5, [1, 0.3, 0.3]),
      makeBody(m, s, 0, 0, G * p1, 0, G * p2, 5, [0.3, 1, 0.3]),
      makeBody(m, 0, 0, 0, -2 * G * p1, 0, -2 * G * p2, 5, [0.3, 0.3, 1]),
    ],
    gravitationalConstant: G,
    softening: 1,
    timeStep: 0.4,
    theta: 0.5,
    trailLength: 500,
    is3D: false,
    scale: 300,
    bloomStrength: 1.2,
  };
}

// ── Galaxy Collision ──────────────────────────────────────────────

function galaxyCollision(): SimulationConfig {
  resetIdCounter();
  const G = 0.5;
  const coreMass = 80000;
  const numStars = 1500;

  const bodies: ReturnType<typeof makeBody>[] = [];

  // Galaxy 1 — centered at (-300, 0, 0), moving right
  const g1Core = makeBody(coreMass, -300, 0, 0, 8, 0, 2, 6, [1, 0.9, 0.5]);
  bodies.push(g1Core);

  for (let i = 0; i < numStars; i++) {
    const [dx, dy, dz] = randomInDisk(15, 250, 8);
    const dist = Math.sqrt(dx * dx + dz * dz);
    const orbitV = circularVelocity(G, coreMass, dist);
    const angle = Math.atan2(dz, dx);
    bodies.push(makeBody(
      0.1 + Math.random() * 0.5,
      g1Core.x + dx, g1Core.y + dy, g1Core.z + dz,
      g1Core.vx - orbitV * Math.sin(angle),
      g1Core.vy,
      g1Core.vz + orbitV * Math.cos(angle),
      0.4 + Math.random() * 0.3,
      [0.6 + Math.random() * 0.4, 0.5 + Math.random() * 0.3, 0.2 + Math.random() * 0.2]
    ));
  }

  // Galaxy 2 — centered at (350, 50, 0), moving left
  const g2Core = makeBody(coreMass, 350, 50, 0, -6, 0, -2, 6, [0.5, 0.7, 1]);
  bodies.push(g2Core);

  for (let i = 0; i < numStars; i++) {
    const [dx, dy, dz] = randomInDisk(15, 220, 8);
    const dist = Math.sqrt(dx * dx + dz * dz);
    const orbitV = circularVelocity(G, coreMass, dist);
    const angle = Math.atan2(dz, dx);
    bodies.push(makeBody(
      0.1 + Math.random() * 0.5,
      g2Core.x + dx, g2Core.y + dy, g2Core.z + dz,
      g2Core.vx - orbitV * Math.sin(angle),
      g2Core.vy,
      g2Core.vz + orbitV * Math.cos(angle),
      0.4 + Math.random() * 0.3,
      [0.3 + Math.random() * 0.2, 0.4 + Math.random() * 0.3, 0.7 + Math.random() * 0.3]
    ));
  }

  return {
    name: 'Galaxy Collision',
    description: 'Two spiral galaxies on a collision course — watch them merge!',
    bodies,
    gravitationalConstant: G,
    softening: 8,
    timeStep: 0.1,
    theta: 0.6,
    trailLength: 60,
    is3D: true,
    scale: 700,
    bloomStrength: 1.0,
  };
}

// ── Spherical Cluster ─────────────────────────────────────────────

function sphericalCluster(): SimulationConfig {
  resetIdCounter();
  const G = 0.3;
  const numBodies = 2000;
  const clusterRadius = 200;
  const bodies: ReturnType<typeof makeBody>[] = [];

  for (let i = 0; i < numBodies; i++) {
    // Plummer model distribution: r = a / sqrt(u^(-2/3) - 1)
    const a = clusterRadius * 0.4;
    const u = Math.random() * 0.999 + 0.001;
    const r = a / Math.sqrt(Math.pow(u, -2 / 3) - 1);
    const clampedR = Math.min(r, clusterRadius);

    const [x, y, z] = randomOnSphere(clampedR);
    const mass = 1 + Math.random() * 3;

    // Give slight random velocity for dynamics
    const vScale = 0.3;
    const vx = gaussRandom(0, vScale);
    const vy = gaussRandom(0, vScale);
    const vz = gaussRandom(0, vScale);

    const temp = Math.random(); // color temperature
    const color: [number, number, number] = temp > 0.7
      ? [0.8 + Math.random() * 0.2, 0.6 + Math.random() * 0.2, 0.2]
      : temp > 0.3
        ? [0.9, 0.9, 0.8 + Math.random() * 0.2]
        : [0.4 + Math.random() * 0.2, 0.5 + Math.random() * 0.2, 0.9 + Math.random() * 0.1];

    bodies.push(makeBody(mass, x, y, z, vx, vy, vz, 0.3 + Math.random() * 0.4, color));
  }

  return {
    name: 'Globular Cluster',
    description: 'A spherical star cluster with Plummer density profile',
    bodies,
    gravitationalConstant: G,
    softening: 5,
    timeStep: 0.15,
    theta: 0.7,
    trailLength: 30,
    is3D: true,
    scale: 400,
    bloomStrength: 0.8,
  };
}

// ── Lagrange Points ───────────────────────────────────────────────

function lagrangeDemo(): SimulationConfig {
  resetIdCounter();
  const G = 2.0;
  const starMass = 50000;
  const planetMass = 500;
  const dist = 200;
  const v = circularVelocity(G, starMass, dist);

  const bodies = [
    makeBody(starMass, 0, 0, 0, 0, 0, 0, 8, [1, 0.95, 0.6], true),
    // Main planet
    makeBody(planetMass, dist, 0, 0, 0, 0, v, 4, [0.2, 0.5, 1]),
    // Trojan asteroids at L4 and L5 (60° ahead and behind)
  ];

  const l4Angle = Math.PI / 3;
  const l5Angle = -Math.PI / 3;

  // Trojan cluster at L4
  for (let i = 0; i < 40; i++) {
    const r = dist + gaussRandom(0, 15);
    const a = l4Angle + gaussRandom(0, 0.08);
    const orbV = circularVelocity(G, starMass, r);
    bodies.push(makeBody(
      0.05, r * Math.cos(a), gaussRandom(0, 2), r * Math.sin(a),
      -orbV * Math.sin(a), 0, orbV * Math.cos(a),
      0.6, [0.3, 0.9, 0.4]
    ));
  }

  // Trojan cluster at L5
  for (let i = 0; i < 40; i++) {
    const r = dist + gaussRandom(0, 15);
    const a = l5Angle + gaussRandom(0, 0.08);
    const orbV = circularVelocity(G, starMass, r);
    bodies.push(makeBody(
      0.05, r * Math.cos(a), gaussRandom(0, 2), r * Math.sin(a),
      -orbV * Math.sin(a), 0, orbV * Math.cos(a),
      0.6, [0.9, 0.4, 0.3]
    ));
  }

  return {
    name: 'Lagrange Points',
    description: 'Trojan asteroids at L4 and L5 points of a star-planet system',
    bodies,
    gravitationalConstant: G,
    softening: 4,
    timeStep: 0.04,
    theta: 0.5,
    trailLength: 500,
    is3D: false,
    scale: 350,
    bloomStrength: 1.3,
  };
}

// ── Chaotic Three Body ────────────────────────────────────────────

function chaoticThreeBody(): SimulationConfig {
  resetIdCounter();
  const G = 1.0;
  const m = 2000;
  const s = 100;

  return {
    name: 'Three-Body Chaos',
    description: 'Three equal masses in a chaotic dance — sensitive to initial conditions',
    bodies: [
      makeBody(m, s, 0, 0, 0, 0, -0.15, 5, [1, 0.2, 0.3]),
      makeBody(m, -s * 0.5, 0, s * 0.866, 0.12, 0, 0.08, 5, [0.2, 1, 0.3]),
      makeBody(m, -s * 0.5, 0, -s * 0.866, -0.12, 0, 0.08, 5, [0.3, 0.3, 1]),
    ],
    gravitationalConstant: G,
    softening: 2,
    timeStep: 0.3,
    theta: 0.5,
    trailLength: 600,
    is3D: false,
    scale: 300,
    bloomStrength: 1.5,
  };
}

// ── Disk Galaxy ───────────────────────────────────────────────────

function diskGalaxy(): SimulationConfig {
  resetIdCounter();
  const G = 0.5;
  const coreMass = 100000;
  const numStars = 3000;

  const bodies: ReturnType<typeof makeBody>[] = [];

  // Central supermassive body
  bodies.push(makeBody(coreMass, 0, 0, 0, 0, 0, 0, 5, [1, 0.95, 0.8], true));

  for (let i = 0; i < numStars; i++) {
    const [dx, dy, dz] = randomInDisk(20, 350, 4);
    const dist = Math.sqrt(dx * dx + dz * dz);
    const orbitV = circularVelocity(G, coreMass, Math.max(dist, 20));
    const angle = Math.atan2(dz, dx);

    // Color based on distance — inner stars hotter (blue-white), outer cooler (red-orange)
    const t = dist / 350;
    const color: [number, number, number] = t < 0.3
      ? [0.7 + Math.random() * 0.3, 0.8 + Math.random() * 0.2, 1]
      : t < 0.6
        ? [1, 0.9 + Math.random() * 0.1, 0.7 + Math.random() * 0.2]
        : [1, 0.4 + Math.random() * 0.3, 0.1 + Math.random() * 0.2];

    bodies.push(makeBody(
      0.1 + Math.random() * 0.3,
      dx, dy, dz,
      -orbitV * Math.sin(angle) + gaussRandom(0, 0.1),
      gaussRandom(0, 0.05),
      orbitV * Math.cos(angle) + gaussRandom(0, 0.1),
      0.2 + Math.random() * 0.3,
      color
    ));
  }

  return {
    name: 'Spiral Galaxy',
    description: 'A disk galaxy with 3000 stars — watch spiral arms form!',
    bodies,
    gravitationalConstant: G,
    softening: 6,
    timeStep: 0.08,
    theta: 0.7,
    trailLength: 20,
    is3D: true,
    scale: 500,
    bloomStrength: 0.7,
  };
}

// ── Pythagorean Three-Body ────────────────────────────────────────

function pythagorean(): SimulationConfig {
  resetIdCounter();
  const G = 1.0;
  const s = 80;

  return {
    name: 'Pythagorean Problem',
    description: 'Three bodies at vertices of a 3-4-5 right triangle — famously chaotic',
    bodies: [
      makeBody(3000, s * 1, 0, s * 3, 0, 0, 0, 5, [1, 0.3, 0.1]),
      makeBody(4000, s * -2, 0, s * -1, 0, 0, 0, 5.5, [0.1, 0.8, 0.3]),
      makeBody(5000, s * 1, 0, s * -1, 0, 0, 0, 6, [0.2, 0.3, 1]),
    ],
    gravitationalConstant: G,
    softening: 1.5,
    timeStep: 0.15,
    theta: 0.5,
    trailLength: 800,
    is3D: false,
    scale: 400,
    bloomStrength: 1.4,
  };
}

// ── Export all presets ─────────────────────────────────────────────

export type PresetFactory = () => SimulationConfig;

export const PRESETS: { id: string; factory: PresetFactory }[] = [
  { id: 'solar-system', factory: solarSystem },
  { id: 'binary-stars', factory: binaryStars },
  { id: 'galaxy-collision', factory: galaxyCollision },
  { id: 'disk-galaxy', factory: diskGalaxy },
  { id: 'globular-cluster', factory: sphericalCluster },
  { id: 'figure-eight', factory: figureEight },
  { id: 'three-body-chaos', factory: chaoticThreeBody },
  { id: 'lagrange-points', factory: lagrangeDemo },
  { id: 'pythagorean', factory: pythagorean },
];
