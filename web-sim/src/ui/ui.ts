import { PRESETS } from '../presets/index';

export interface UICallbacks {
  onPresetSelect: (presetId: string) => void;
  onSpeedChange: (multiplier: number) => void;
  onTogglePause: () => void;
  onToggleTrails: (enabled: boolean) => void;
  onReset: () => void;
}

export class UI {
  private container: HTMLElement;
  private statsEl: HTMLElement;
  private fpsEl: HTMLElement;
  private bodyCountEl: HTMLElement;
  private timeEl: HTMLElement;
  private energyEl: HTMLElement;
  private pauseBtn: HTMLButtonElement;
  private paused = false;
  private callbacks: UICallbacks;

  constructor(callbacks: UICallbacks) {
    this.callbacks = callbacks;
    this.container = document.getElementById('ui-panel')!;

    // Build UI
    this.container.innerHTML = `
      <div class="ui-header">
        <h1 class="ui-title">N-Body</h1>
        <p class="ui-subtitle">Gravitational Simulations</p>
      </div>

      <div class="ui-section">
        <label class="ui-label">Simulation</label>
        <div class="preset-grid" id="preset-grid"></div>
      </div>

      <div class="ui-section">
        <label class="ui-label">Controls</label>
        <div class="controls-row">
          <button class="btn btn-icon" id="btn-pause" title="Pause/Resume">
            <svg id="icon-pause" width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
              <rect x="6" y="4" width="4" height="16"/>
              <rect x="14" y="4" width="4" height="16"/>
            </svg>
            <svg id="icon-play" width="18" height="18" viewBox="0 0 24 24" fill="currentColor" style="display:none">
              <polygon points="5,3 19,12 5,21"/>
            </svg>
          </button>
          <button class="btn btn-icon" id="btn-reset" title="Reset">
            <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
              <path d="M1 4v6h6"/><path d="M3.51 15a9 9 0 1 0 2.13-9.36L1 10"/>
            </svg>
          </button>
        </div>
      </div>

      <div class="ui-section">
        <label class="ui-label">Speed</label>
        <div class="slider-row">
          <input type="range" id="speed-slider" min="-2" max="3" step="0.1" value="0" class="slider"/>
          <span id="speed-value" class="slider-value">1x</span>
        </div>
      </div>

      <div class="ui-section">
        <label class="ui-label">Trails</label>
        <label class="toggle">
          <input type="checkbox" id="trails-toggle" checked/>
          <span class="toggle-slider"></span>
        </label>
      </div>

      <div class="ui-section ui-stats" id="stats-section">
        <label class="ui-label">Stats</label>
        <div class="stat-row"><span class="stat-label">FPS</span><span id="stat-fps" class="stat-value">0</span></div>
        <div class="stat-row"><span class="stat-label">Bodies</span><span id="stat-bodies" class="stat-value">0</span></div>
        <div class="stat-row"><span class="stat-label">Time</span><span id="stat-time" class="stat-value">0</span></div>
        <div class="stat-row"><span class="stat-label">Energy</span><span id="stat-energy" class="stat-value">0</span></div>
      </div>

      <div class="ui-footer">
        <p>Drag to orbit · Scroll to zoom</p>
        <p>Space to pause · R to reset</p>
      </div>
    `;

    // Populate presets
    const grid = document.getElementById('preset-grid')!;
    for (const preset of PRESETS) {
      const config = preset.factory();
      const btn = document.createElement('button');
      btn.className = 'preset-btn';
      btn.dataset.preset = preset.id;
      btn.innerHTML = `
        <span class="preset-name">${config.name}</span>
        <span class="preset-count">${config.bodies.length} bodies</span>
      `;
      btn.addEventListener('click', () => {
        this.selectPreset(preset.id);
        callbacks.onPresetSelect(preset.id);
      });
      grid.appendChild(btn);
    }

    // Controls
    this.pauseBtn = document.getElementById('btn-pause') as HTMLButtonElement;
    this.pauseBtn.addEventListener('click', () => this.togglePause());

    document.getElementById('btn-reset')!.addEventListener('click', () => callbacks.onReset());

    const speedSlider = document.getElementById('speed-slider') as HTMLInputElement;
    const speedValue = document.getElementById('speed-value')!;
    speedSlider.addEventListener('input', () => {
      const exp = parseFloat(speedSlider.value);
      const multiplier = Math.pow(10, exp);
      speedValue.textContent = multiplier >= 1
        ? `${multiplier.toFixed(multiplier >= 10 ? 0 : 1)}x`
        : `${multiplier.toFixed(2)}x`;
      callbacks.onSpeedChange(multiplier);
    });

    const trailsToggle = document.getElementById('trails-toggle') as HTMLInputElement;
    trailsToggle.addEventListener('change', () => {
      callbacks.onToggleTrails(trailsToggle.checked);
    });

    // Stats elements
    this.statsEl = document.getElementById('stats-section')!;
    this.fpsEl = document.getElementById('stat-fps')!;
    this.bodyCountEl = document.getElementById('stat-bodies')!;
    this.timeEl = document.getElementById('stat-time')!;
    this.energyEl = document.getElementById('stat-energy')!;

    // Keyboard shortcuts
    window.addEventListener('keydown', (e) => {
      if (e.code === 'Space') {
        e.preventDefault();
        this.togglePause();
      }
      if (e.code === 'KeyR') {
        callbacks.onReset();
      }
    });

    // Select first preset
    this.selectPreset(PRESETS[0].id);
  }

  selectPreset(id: string): void {
    const buttons = this.container.querySelectorAll('.preset-btn');
    buttons.forEach(btn => {
      btn.classList.toggle('active', (btn as HTMLElement).dataset.preset === id);
    });
  }

  togglePause(): void {
    this.paused = !this.paused;
    document.getElementById('icon-pause')!.style.display = this.paused ? 'none' : 'block';
    document.getElementById('icon-play')!.style.display = this.paused ? 'block' : 'none';
    this.callbacks.onTogglePause();
  }

  updateStats(fps: number, bodyCount: number, time: number, energy: number): void {
    this.fpsEl.textContent = fps.toFixed(0);
    this.bodyCountEl.textContent = bodyCount.toLocaleString();
    this.timeEl.textContent = time.toFixed(1);
    this.energyEl.textContent = energy.toExponential(2);
  }
}
