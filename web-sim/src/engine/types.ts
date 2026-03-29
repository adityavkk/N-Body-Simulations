/** A body in the simulation */
export interface Body {
  id: number;
  mass: number;
  x: number;
  y: number;
  z: number;
  vx: number;
  vy: number;
  vz: number;
  radius: number;
  color: [number, number, number]; // RGB 0-1
  trail: { x: number; y: number; z: number }[];
  fixed?: boolean; // if true, body doesn't move (e.g. central star)
}

export interface SimulationConfig {
  name: string;
  description: string;
  bodies: Body[];
  gravitationalConstant: number;
  softening: number;
  timeStep: number;
  theta: number; // Barnes-Hut accuracy parameter
  trailLength: number;
  is3D: boolean;
  scale: number; // camera distance multiplier
  bloomStrength: number;
}
