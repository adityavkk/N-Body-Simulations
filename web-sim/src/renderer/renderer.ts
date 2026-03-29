import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
import { EffectComposer } from 'three/addons/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/addons/postprocessing/RenderPass.js';
import { UnrealBloomPass } from 'three/addons/postprocessing/UnrealBloomPass.js';
import type { Body, SimulationConfig } from '../engine/types';

const TRAIL_OPACITY = 0.35;

export class Renderer {
  private canvas: HTMLCanvasElement;
  private renderer: THREE.WebGLRenderer;
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private controls: OrbitControls;
  private composer: EffectComposer;
  private bloomPass: UnrealBloomPass;

  // Body meshes
  private bodyMeshes: Map<number, THREE.Mesh> = new Map();
  private bodyMaterial: Map<number, THREE.MeshBasicMaterial> = new Map();

  // Trail lines
  private trailLines: Map<number, THREE.Line> = new Map();
  private trailGeometries: Map<number, THREE.BufferGeometry> = new Map();

  // Point cloud for large simulations
  private usePointCloud = false;
  private pointCloud: THREE.Points | null = null;
  private pointPositions: Float32Array | null = null;
  private pointColors: Float32Array | null = null;
  private pointSizes: Float32Array | null = null;

  private cameraScale = 1;

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas;

    this.renderer = new THREE.WebGLRenderer({
      canvas,
      antialias: true,
      alpha: true,
    });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    this.renderer.toneMapping = THREE.ACESFilmicToneMapping;
    this.renderer.toneMappingExposure = 1.2;

    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x020208);

    // Subtle ambient starfield
    this.addStarfield();

    this.camera = new THREE.PerspectiveCamera(60, 1, 0.1, 100000);
    this.camera.position.set(0, 300, 600);

    this.controls = new OrbitControls(this.camera, canvas);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.minDistance = 10;
    this.controls.maxDistance = 50000;

    // Post-processing
    this.composer = new EffectComposer(this.renderer);
    this.composer.addPass(new RenderPass(this.scene, this.camera));

    this.bloomPass = new UnrealBloomPass(
      new THREE.Vector2(window.innerWidth, window.innerHeight),
      1.5, // strength
      0.4, // radius
      0.85 // threshold
    );
    this.composer.addPass(this.bloomPass);

    this.handleResize();
    window.addEventListener('resize', () => this.handleResize());
  }

  private addStarfield(): void {
    const starCount = 3000;
    const positions = new Float32Array(starCount * 3);
    const colors = new Float32Array(starCount * 3);

    for (let i = 0; i < starCount; i++) {
      const r = 5000 + Math.random() * 30000;
      const theta = Math.random() * Math.PI * 2;
      const phi = Math.acos(2 * Math.random() - 1);
      positions[i * 3] = r * Math.sin(phi) * Math.cos(theta);
      positions[i * 3 + 1] = r * Math.sin(phi) * Math.sin(theta);
      positions[i * 3 + 2] = r * Math.cos(phi);

      const brightness = 0.3 + Math.random() * 0.7;
      colors[i * 3] = brightness;
      colors[i * 3 + 1] = brightness;
      colors[i * 3 + 2] = brightness * (0.8 + Math.random() * 0.2);
    }

    const geo = new THREE.BufferGeometry();
    geo.setAttribute('position', new THREE.BufferAttribute(positions, 3));
    geo.setAttribute('color', new THREE.BufferAttribute(colors, 3));

    const mat = new THREE.PointsMaterial({
      size: 1.5,
      vertexColors: true,
      transparent: true,
      opacity: 0.8,
      sizeAttenuation: false,
    });

    this.scene.add(new THREE.Points(geo, mat));
  }

  private handleResize(): void {
    const w = window.innerWidth;
    const h = window.innerHeight;
    this.renderer.setSize(w, h);
    this.composer.setSize(w, h);
    this.camera.aspect = w / h;
    this.camera.updateProjectionMatrix();
  }

  /** Setup scene for a new simulation config */
  setup(config: SimulationConfig): void {
    // Clear existing bodies
    for (const mesh of this.bodyMeshes.values()) this.scene.remove(mesh);
    for (const line of this.trailLines.values()) this.scene.remove(line);
    if (this.pointCloud) {
      this.scene.remove(this.pointCloud);
      this.pointCloud = null;
    }
    this.bodyMeshes.clear();
    this.bodyMaterial.clear();
    this.trailLines.clear();
    this.trailGeometries.clear();

    this.bloomPass.strength = config.bloomStrength;
    this.cameraScale = config.scale;

    // Use point cloud for large body counts
    this.usePointCloud = config.bodies.length > 200;

    if (this.usePointCloud) {
      this.setupPointCloud(config.bodies);
    } else {
      this.setupMeshBodies(config.bodies);
    }

    // Reset camera
    const d = config.scale;
    if (config.is3D) {
      this.camera.position.set(d * 0.5, d * 0.6, d * 1.0);
    } else {
      this.camera.position.set(0, d * 1.3, 0.01);
    }
    this.controls.target.set(0, 0, 0);
    this.controls.update();
  }

  private setupMeshBodies(bodies: Body[]): void {
    for (const body of bodies) {
      const geo = new THREE.SphereGeometry(body.radius, 16, 12);
      const mat = new THREE.MeshBasicMaterial({
        color: new THREE.Color(body.color[0], body.color[1], body.color[2]),
      });
      const mesh = new THREE.Mesh(geo, mat);
      mesh.position.set(body.x, body.y, body.z);
      this.scene.add(mesh);
      this.bodyMeshes.set(body.id, mesh);
      this.bodyMaterial.set(body.id, mat);

      // Trail
      const trailGeo = new THREE.BufferGeometry();
      const trailMat = new THREE.LineBasicMaterial({
        color: new THREE.Color(body.color[0], body.color[1], body.color[2]),
        transparent: true,
        opacity: TRAIL_OPACITY,
      });
      const trailLine = new THREE.Line(trailGeo, trailMat);
      this.scene.add(trailLine);
      this.trailLines.set(body.id, trailLine);
      this.trailGeometries.set(body.id, trailGeo);
    }
  }

  private setupPointCloud(bodies: Body[]): void {
    const n = bodies.length;
    this.pointPositions = new Float32Array(n * 3);
    this.pointColors = new Float32Array(n * 3);
    this.pointSizes = new Float32Array(n);

    for (let i = 0; i < n; i++) {
      const b = bodies[i];
      this.pointPositions[i * 3] = b.x;
      this.pointPositions[i * 3 + 1] = b.y;
      this.pointPositions[i * 3 + 2] = b.z;
      this.pointColors[i * 3] = b.color[0];
      this.pointColors[i * 3 + 1] = b.color[1];
      this.pointColors[i * 3 + 2] = b.color[2];
      this.pointSizes[i] = b.radius * 2;
    }

    const geo = new THREE.BufferGeometry();
    geo.setAttribute('position', new THREE.BufferAttribute(this.pointPositions, 3));
    geo.setAttribute('color', new THREE.BufferAttribute(this.pointColors, 3));
    geo.setAttribute('size', new THREE.BufferAttribute(this.pointSizes, 1));

    const mat = new THREE.ShaderMaterial({
      uniforms: {
        pixelRatio: { value: Math.min(window.devicePixelRatio, 2) },
      },
      vertexShader: `
        attribute float size;
        varying vec3 vColor;
        uniform float pixelRatio;
        void main() {
          vColor = color;
          vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
          gl_PointSize = size * pixelRatio * (300.0 / -mvPosition.z);
          gl_PointSize = clamp(gl_PointSize, 1.0, 40.0);
          gl_Position = projectionMatrix * mvPosition;
        }
      `,
      fragmentShader: `
        varying vec3 vColor;
        void main() {
          vec2 center = gl_PointCoord - vec2(0.5);
          float dist = length(center);
          if (dist > 0.5) discard;
          float alpha = 1.0 - smoothstep(0.2, 0.5, dist);
          float glow = exp(-dist * 4.0) * 0.5;
          gl_FragColor = vec4(vColor * (1.0 + glow), alpha);
        }
      `,
      transparent: true,
      vertexColors: true,
      depthWrite: false,
      blending: THREE.AdditiveBlending,
    });

    this.pointCloud = new THREE.Points(geo, mat);
    this.scene.add(this.pointCloud);
  }

  /** Update rendering for current body positions */
  update(bodies: Body[]): void {
    if (this.usePointCloud) {
      this.updatePointCloud(bodies);
    } else {
      this.updateMeshBodies(bodies);
    }

    this.controls.update();
    this.composer.render();
  }

  private updateMeshBodies(bodies: Body[]): void {
    for (const body of bodies) {
      const mesh = this.bodyMeshes.get(body.id);
      if (mesh) {
        mesh.position.set(body.x, body.y, body.z);
      }

      // Update trail
      const trailGeo = this.trailGeometries.get(body.id);
      if (trailGeo && body.trail.length > 1) {
        const positions = new Float32Array(body.trail.length * 3);
        for (let i = 0; i < body.trail.length; i++) {
          positions[i * 3] = body.trail[i].x;
          positions[i * 3 + 1] = body.trail[i].y;
          positions[i * 3 + 2] = body.trail[i].z;
        }
        trailGeo.setAttribute('position', new THREE.BufferAttribute(positions, 3));
      }
    }
  }

  private updatePointCloud(bodies: Body[]): void {
    if (!this.pointPositions || !this.pointCloud) return;

    for (let i = 0; i < bodies.length; i++) {
      this.pointPositions[i * 3] = bodies[i].x;
      this.pointPositions[i * 3 + 1] = bodies[i].y;
      this.pointPositions[i * 3 + 2] = bodies[i].z;
    }

    const posAttr = this.pointCloud.geometry.getAttribute('position') as THREE.BufferAttribute;
    posAttr.needsUpdate = true;
  }

  dispose(): void {
    this.renderer.dispose();
    for (const mesh of this.bodyMeshes.values()) {
      mesh.geometry.dispose();
    }
    if (this.pointCloud) {
      this.pointCloud.geometry.dispose();
    }
  }
}
