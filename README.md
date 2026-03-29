# N-Body Simulations

Beautiful real-time gravitational N-body simulations featuring the Barnes-Hut octree algorithm, rendered with Three.js and WebGL.

![N-Body Simulations](images/sphericalGalaxy.gif)

## Features

- **9 simulation presets** — Solar System, Galaxy Collision, Spiral Galaxy, Globular Cluster, Binary Stars, Figure-8, Three-Body Chaos, Lagrange Points, Pythagorean Problem
- **Barnes-Hut octree** — O(n log n) force approximation, simulates thousands of bodies in real-time
- **Velocity Verlet integration** — superior energy conservation over Euler methods
- **3D visualization** — Three.js with bloom post-processing, additive blending, and particle glow effects
- **Interactive controls** — orbit camera, zoom, speed slider, pause/resume, trail toggle
- **Beautiful UI** — glassmorphism panel with real-time stats (FPS, body count, energy)
- **Responsive** — works on desktop and mobile

## Quick Start

```bash
cd web-sim
npm install
npm run dev
```

Then open [http://localhost:5173](http://localhost:5173).

## Simulations

| Preset | Bodies | Description |
|--------|--------|-------------|
| Solar System | 9 | 8 planets orbiting a central star |
| Binary Stars | 82 | Two stars with debris disks |
| Galaxy Collision | 3002 | Two spiral galaxies merging |
| Spiral Galaxy | 3001 | Disk galaxy with spiral arm formation |
| Globular Cluster | 2000 | Spherical cluster with Plummer density |
| Figure Eight | 3 | Famous periodic three-body choreography |
| Three-Body Chaos | 3 | Chaotic gravitational dance |
| Lagrange Points | 83 | Trojan asteroids at L4 and L5 |
| Pythagorean Problem | 3 | 3-4-5 triangle initial conditions |

## Architecture

```
web-sim/
├── src/
│   ├── engine/          # Physics simulation
│   │   ├── types.ts     # Body, SimulationConfig types
│   │   ├── octree.ts    # Barnes-Hut octree (3D)
│   │   └── simulation.ts # Velocity Verlet integrator
│   ├── presets/         # Simulation presets
│   │   ├── helpers.ts   # Body creation, sampling utilities
│   │   └── index.ts     # All 9 preset configurations
│   ├── renderer/        # Three.js visualization
│   │   └── renderer.ts  # WebGL renderer with bloom
│   ├── ui/              # User interface
│   │   └── ui.ts        # Controls panel
│   ├── main.ts          # Application entry point
│   └── style.css        # Styles
├── index.html
├── package.json
├── tsconfig.json
└── vite.config.ts
```

## Tech Stack

- **TypeScript** — strict mode, modern ES2022 target
- **Three.js** — 3D rendering with WebGL
- **Vite** — instant dev server and optimized builds
- **Barnes-Hut Algorithm** — O(n log n) gravitational force approximation
- **Velocity Verlet** — symplectic integrator for accurate orbital mechanics

## Controls

| Input | Action |
|-------|--------|
| Drag | Orbit camera |
| Scroll | Zoom in/out |
| Space | Pause/resume |
| R | Reset simulation |

## Legacy Haskell Implementations

The original Haskell implementations are preserved in `Direct-Simulation/` (O(n²)) and `Barnes-Hut/` (O(n log n) quadtree).

## License

BSD-3-Clause
