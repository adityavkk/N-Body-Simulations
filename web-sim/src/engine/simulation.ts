import type { Body, SimulationConfig } from './types';
import { buildOctree, calcAcceleration } from './octree';

export class Simulation {
  bodies: Body[];
  config: SimulationConfig;
  time: number = 0;
  paused: boolean = false;
  speedMultiplier: number = 1;
  private _stepCount: number = 0;

  constructor(config: SimulationConfig) {
    this.config = config;
    this.bodies = config.bodies.map(b => ({
      ...b,
      trail: [],
    }));
  }

  /** Leapfrog (Velocity Verlet) integration — much better energy conservation than Euler */
  step(): void {
    if (this.paused) return;

    const dt = this.config.timeStep * this.speedMultiplier;
    const G = this.config.gravitationalConstant;
    const theta = this.config.theta;
    const softening = this.config.softening;
    const n = this.bodies.length;

    // Build octree
    const tree = buildOctree(this.bodies);
    if (!tree) return;

    // Calculate accelerations at current positions
    const accel = new Float64Array(n * 3);
    for (let i = 0; i < n; i++) {
      const body = this.bodies[i];
      if (body.fixed) continue;
      const [ax, ay, az] = calcAcceleration(body, tree, G, theta, softening);
      accel[i * 3] = ax;
      accel[i * 3 + 1] = ay;
      accel[i * 3 + 2] = az;
    }

    // Update positions using current velocities + half-step acceleration
    for (let i = 0; i < n; i++) {
      const body = this.bodies[i];
      if (body.fixed) continue;
      body.x += body.vx * dt + 0.5 * accel[i * 3] * dt * dt;
      body.y += body.vy * dt + 0.5 * accel[i * 3 + 1] * dt * dt;
      body.z += body.vz * dt + 0.5 * accel[i * 3 + 2] * dt * dt;
    }

    // Build new octree at updated positions
    const tree2 = buildOctree(this.bodies);
    if (!tree2) return;

    // Calculate new accelerations
    const accel2 = new Float64Array(n * 3);
    for (let i = 0; i < n; i++) {
      const body = this.bodies[i];
      if (body.fixed) continue;
      const [ax, ay, az] = calcAcceleration(body, tree2, G, theta, softening);
      accel2[i * 3] = ax;
      accel2[i * 3 + 1] = ay;
      accel2[i * 3 + 2] = az;
    }

    // Update velocities using average of old and new accelerations
    for (let i = 0; i < n; i++) {
      const body = this.bodies[i];
      if (body.fixed) continue;
      body.vx += 0.5 * (accel[i * 3] + accel2[i * 3]) * dt;
      body.vy += 0.5 * (accel[i * 3 + 1] + accel2[i * 3 + 1]) * dt;
      body.vz += 0.5 * (accel[i * 3 + 2] + accel2[i * 3 + 2]) * dt;
    }

    // Update trails
    this._stepCount++;
    if (this._stepCount % 2 === 0) {
      for (const body of this.bodies) {
        body.trail.push({ x: body.x, y: body.y, z: body.z });
        if (body.trail.length > this.config.trailLength) {
          body.trail.shift();
        }
      }
    }

    this.time += dt;
  }

  reset(config: SimulationConfig): void {
    this.config = config;
    this.bodies = config.bodies.map(b => ({
      ...b,
      trail: [],
    }));
    this.time = 0;
    this._stepCount = 0;
  }

  get bodyCount(): number {
    return this.bodies.length;
  }

  /** Calculate total kinetic energy */
  get kineticEnergy(): number {
    let ke = 0;
    for (const b of this.bodies) {
      ke += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
    }
    return ke;
  }
}
