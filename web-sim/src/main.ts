import { Simulation } from './engine/simulation';
import { Renderer } from './renderer/renderer';
import { UI } from './ui/ui';
import { PRESETS } from './presets/index';
import './style.css';

// ── Bootstrap ─────────────────────────────────────────────────────

const canvas = document.getElementById('canvas') as HTMLCanvasElement;
const renderer = new Renderer(canvas);

let currentPresetId = PRESETS[0].id;
let simulation: Simulation;
let trailsEnabled = true;

function loadPreset(id: string): void {
  const preset = PRESETS.find(p => p.id === id);
  if (!preset) return;
  currentPresetId = id;
  const config = preset.factory();
  config.trailLength = trailsEnabled ? config.trailLength : 0;
  simulation = new Simulation(config);
  renderer.setup(config);
}

// Initialize simulation
loadPreset(currentPresetId);

// ── UI ────────────────────────────────────────────────────────────

const ui = new UI({
  onPresetSelect: (id) => {
    loadPreset(id);
  },
  onSpeedChange: (multiplier) => {
    simulation.speedMultiplier = multiplier;
  },
  onTogglePause: () => {
    simulation.paused = !simulation.paused;
  },
  onToggleTrails: (enabled) => {
    trailsEnabled = enabled;
    simulation.config.trailLength = enabled ? 300 : 0;
    if (!enabled) {
      for (const body of simulation.bodies) {
        body.trail = [];
      }
    }
  },
  onReset: () => {
    loadPreset(currentPresetId);
    ui.selectPreset(currentPresetId);
  },
});

// ── Main Loop ─────────────────────────────────────────────────────

let lastTime = performance.now();
let frameCount = 0;
let fps = 60;
let fpsTimer = 0;

// Determine substeps based on body count
function getSubSteps(): number {
  const n = simulation.bodyCount;
  if (n > 2000) return 1;
  if (n > 500) return 2;
  return 3;
}

function animate(time: number): void {
  requestAnimationFrame(animate);

  const dt = time - lastTime;
  lastTime = time;

  // FPS counter
  frameCount++;
  fpsTimer += dt;
  if (fpsTimer >= 500) {
    fps = (frameCount / fpsTimer) * 1000;
    frameCount = 0;
    fpsTimer = 0;
    ui.updateStats(fps, simulation.bodyCount, simulation.time, simulation.kineticEnergy);
  }

  // Physics substeps
  const subSteps = getSubSteps();
  for (let i = 0; i < subSteps; i++) {
    simulation.step();
  }

  // Render
  renderer.update(simulation.bodies);
}

requestAnimationFrame(animate);
