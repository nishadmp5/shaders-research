import { Sphere, shaderMaterial } from "@react-three/drei";
import { extend, useFrame } from "@react-three/fiber";
import { useRef } from "react";
import * as THREE from "three";
import { useControls } from "leva";

const DistortedFluidMaterial = shaderMaterial(
  {
    uTime: 0,
    uSpeed: 0.6,          // Texture flow speed
    uDistortion: 0.0,
    uFrequency: 3.0,
    uIntensity: 1.0,
    
    // Structural Uniforms
    uNoiseDensity: 2.0,
    uWarpStrength: 1.0,
    uContrast: 0.5,
    uMinWidth: 0.1,
    uMaxWidth: 1.0,
    
    // --- BACKLIGHT UNIFORMS ---
    uRimPower: 8.0,       
    uRimStrength: 1.0,    
    uInnerDarkness: 0.8,  
    uRimColor: new THREE.Color("#8AAFFF"), 
    
    // --- NEW: WANDERING FRESNEL ---
    uRimWanderSpeed: 1.5,  // How fast the light moves
    uRimWanderJitter: 0.3, // How far it moves from the center
    
    uColorA: new THREE.Color("#E048D7"),
    uColorB: new THREE.Color("#5FCCFE"),
    uColorC: new THREE.Color("#521554"),
  },
  // --- VERTEX SHADER ---
  `
    uniform float uTime;
    uniform float uDistortion;
    uniform float uSpeed;
    uniform float uFrequency;

    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;

    // Simplex Noise
    vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
    vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }
    
    float snoise(vec3 v) {
        const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
        const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);
        vec3 i  = floor(v + dot(v, C.yyy) );
        vec3 x0 = v - i + dot(i, C.xxx) ;
        vec3 g = step(x0.yzx, x0.xyz);
        vec3 l = 1.0 - g;
        vec3 i1 = min( g.xyz, l.zxy );
        vec3 i2 = max( g.xyz, l.zxy );
        vec3 x1 = x0 - i1 + C.xxx;
        vec3 x2 = x0 - i2 + C.yyy;
        vec3 x3 = x0 - D.yyy;
        i = mod289(i);
        vec4 p = permute( permute( permute(
                    i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
                    + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
                    + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));
        float n_ = 0.142857142857;
        vec3  ns = n_ * D.wyz - D.xzx;
        vec4 j = p - 49.0 * floor(p * ns.z * ns.z);
        vec4 x_ = floor(j * ns.z);
        vec4 y_ = floor(j - 7.0 * x_ );
        vec4 x = x_ *ns.x + ns.yyyy;
        vec4 y = y_ *ns.x + ns.yyyy;
        vec4 h = 1.0 - abs(x) - abs(y);
        vec4 b0 = vec4( x.xy, y.xy );
        vec4 b1 = vec4( x.zw, y.zw );
        vec4 s0 = floor(b0)*2.0 + 1.0;
        vec4 s1 = floor(b1)*2.0 + 1.0;
        vec4 sh = -step(h, vec4(0.0));
        vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
        vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;
        vec3 p0 = vec3(a0.xy,h.x);
        vec3 p1 = vec3(a0.zw,h.y);
        vec3 p2 = vec3(a1.xy,h.z);
        vec3 p3 = vec3(a1.zw,h.w);
        vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
        p0 *= norm.x;
        p1 *= norm.y;
        p2 *= norm.z;
        p3 *= norm.w;
        vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
        m = m * m;
        return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), dot(p2,x2), dot(p3,x3) ) );
    }

    void main() {
        vUv = uv;
        vViewNormal = normalize(normalMatrix * normal);
        vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
        vViewPosition = -mvPosition.xyz;

        float noise = snoise(vec3(position * uFrequency + uTime * uSpeed));
        vNoise = noise;
        vec3 deformedPos = position + (normal * noise * uDistortion);
        gl_Position = projectionMatrix * modelViewMatrix * vec4(deformedPos, 1.0);
    }
  `,
  // --- FRAGMENT SHADER ---
  `
    uniform float uTime;
    uniform float uIntensity;
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    
    uniform float uNoiseDensity;
    uniform float uWarpStrength;
    uniform float uContrast;
    uniform float uMinWidth;
    uniform float uMaxWidth;
    
    // Backlight Uniforms
    uniform float uRimPower;
    uniform float uRimStrength;
    uniform float uInnerDarkness;
    uniform vec3 uRimColor;
    
    // Wander Uniforms
    uniform float uRimWanderSpeed;
    uniform float uRimWanderJitter;
    
    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;

    float random (in vec2 st) { return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123); }
    float noise (in vec2 st) {
        vec2 i = floor(st);
        vec2 f = fract(st);
        float a = random(i);
        float b = random(i + vec2(1.0, 0.0));
        float c = random(i + vec2(0.0, 1.0));
        float d = random(i + vec2(1.0, 1.0));
        vec2 u = f * f * (3.0 - 2.0 * f);
        return mix(a, b, u.x) + (c - a)* u.y * (1.0 - u.x) + (d - b) * u.x * u.y;
    }
    float fbm (in vec2 st) {
        float value = 0.0;
        float amplitude = 0.5;
        for (int i = 0; i < 5; i++) {
            value += amplitude * noise(st);
            st *= 2.0;
            amplitude *= 0.5;
        }
        return value;
    }

    void main() {
        vec2 uv = vUv * uNoiseDensity;
        
        // --- FLUID PATTERN ---
        vec2 q = vec2(0.);
        q.x = fbm(uv + 0.1 * uTime);
        q.y = fbm(uv + vec2(1.0));

        vec2 r = vec2(0.);
        r.x = fbm(uv + uWarpStrength * q + vec2(1.7, 9.2) + 0.15 * uTime);
        r.y = fbm(uv + uWarpStrength * q + vec2(8.3, 2.8) + 0.126 * uTime);

        float f = fbm(uv + r);

        // --- FLUID COLORS ---
        vec3 color = mix(uColorA, uColorB, clamp((f*f)*4.0, 0.0, 1.0));
        color = mix(color, uColorC, clamp(length(q), 0.0, 1.0));
        
        float widthMap = smoothstep(0.0, 1.5, length(q)); 
        float currentWidth = mix(uMinWidth, uMaxWidth, widthMap);
        float sharpness = 1.0 / max(currentWidth, 0.001);

        float glowIntensity = pow(f, sharpness) * 4.0;
        vec3 fluidGlow = uColorA * glowIntensity;
        
        vec3 finalColor = (color * uIntensity) + fluidGlow;
        finalColor *= smoothstep(uContrast - 0.4, uContrast + 0.4, f);

        // --- BACKLIGHTING LOGIC (Wandering) ---
        
        vec3 viewDir = normalize(vViewPosition);
        
        // 1. Create a wandering offset vector based on time
        float wanderX = sin(uTime * uRimWanderSpeed) * uRimWanderJitter;
        float wanderY = cos(uTime * uRimWanderSpeed * 0.8) * uRimWanderJitter;
        vec3 wanderOffset = vec3(wanderX, wanderY, 0.0);
        
        // 2. Perturb the normal used for Fresnel calculation
        // This makes the "center" of the sphere appear to drift
        vec3 wanderingNormal = normalize(vViewNormal + wanderOffset);

        // 3. Calculate Fresnel with the wandering normal
        float fresnel = 1.0 - dot(viewDir, wanderingNormal);
        
        // 4. Inner Blocking (Shadow)
        finalColor *= clamp(mix(1.0 - uInnerDarkness, 1.0, fresnel), 0.0, 1.0);

        // 5. Rim Light
        float rimParam = pow(fresnel, uRimPower);
        vec3 rimFinal = uRimColor * rimParam * uRimStrength;
        
        gl_FragColor = vec4(finalColor + rimFinal, 1.0);
    }
  `
);

extend({ DistortedFluidMaterial });

const DistortedFluidSphere = (props) => {
  const materialRef = useRef();

  const { 
    minWidth, maxWidth, 
    noiseDensity, warpStrength, contrast, intensity, 
    rimPower, rimStrength, innerDarkness, rimColor,
    colorA, colorB, colorC,
    speed,
    rimWanderSpeed, rimWanderJitter // New Controls
  } = useControls("Distorted Fluid", {
    minWidth: { value: 0.1, min: 0.01, max: 2.0 },
    maxWidth: { value: 0.5, min: 0.01, max: 2.0 },
    noiseDensity: { value: 5.0, min: 0.1, max: 5.0 },
    warpStrength: { value: 4.0, min: 0.0, max: 5.0 },
    contrast: { value: 0.0, min: 0.0, max: 1.0 },
    intensity: { value: 3, min: 0.0, max: 5.0 },
    speed: { value: 0.6, min: 0.0, max: 2.0, label: "Flow Speed" },
    
    // Backlight Controls
    rimPower: { value: 8.0, min: 0.5, max: 8.0, label: "Backlight Edge Sharpness" },
    rimStrength: { value: 1.0, min: 0.0, max: 5.0, label: "Backlight Brightness" },
    innerDarkness: { value: 0.9, min: 0.0, max: 1.0, label: "Blocking Opacity" },
    rimColor: { value: "#8AAFFF", label: "Backlight Color" },
    
    // Wander Controls
    rimWanderSpeed: { value: 1.5, min: 0.1, max: 3.0, label: "Wander Speed" },
    rimWanderJitter: { value: 0.3, min: 0.0, max: 1.0, label: "Wander Distance" },
    
    colorA: "#e048d7",
    colorB: "#2a7ed5",
    colorC: "#000000",
  });

  useFrame(({ clock }) => {
    if (materialRef.current) {
      materialRef.current.uTime = clock.getElapsedTime();
    }
  });

  return (
    <Sphere args={[1.5, 128, 128]}>
      <distortedFluidMaterial 
        ref={materialRef} 
        uMinWidth={minWidth}
        uMaxWidth={maxWidth}
        uNoiseDensity={noiseDensity}
        uWarpStrength={warpStrength}
        uContrast={contrast}
        uIntensity={intensity}
        uSpeed={speed}
        
        // Backlight Props
        uRimPower={rimPower}
        uRimStrength={rimStrength}
        uInnerDarkness={innerDarkness}
        uRimColor={new THREE.Color(rimColor)}
        
        // Wander Props
        uRimWanderSpeed={rimWanderSpeed}
        uRimWanderJitter={rimWanderJitter}
        
        // Colors
        uColorA={new THREE.Color(colorA)}
        uColorB={new THREE.Color(colorB)}
        uColorC={new THREE.Color(colorC)}
        {...props} 
      />
    </Sphere>
  );
};

export default DistortedFluidSphere;