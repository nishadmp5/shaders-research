import { Sphere, shaderMaterial } from "@react-three/drei";
import { extend, useFrame } from "@react-three/fiber";
import { useRef } from "react";
import * as THREE from "three";

const DistortedFluidMaterial = shaderMaterial(
  {
    uTime: 0,
    uSpeed: 0.2,
    uDistortion: 0.01,
    uFrequency: 1.0,
    uIntensity: 1.0,
    
    // --- NEW STRUCTURAL UNIFORMS ---
    uNoiseDensity: 10.0,     // Controls the zoom level of the noise (was 3.0)
    uWarpStrength: 1.0,     // Controls how "swirly" the pattern is (was 1.0)
    uGlowPower: 1.0,        // Controls sharpness of the veins (was 3.0)
    uContrast: 0.9,         // Controls how much black space there is (via smoothstep)
    
    uColorA: new THREE.Color("#E048D7"),
    uColorB: new THREE.Color("#5FCCFE"),
    uColorC: new THREE.Color("#521554"),
  },
  // --- VERTEX SHADER (Unchanged) ---
  `
    uniform float uTime;
    uniform float uDistortion;
    uniform float uSpeed;
    uniform float uFrequency;

    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vNormal;

    // Standard Simplex Noise
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
        vNormal = normal;
        float noise = snoise(vec3(position * uFrequency + uTime * uSpeed));
        vNoise = noise;
        vec3 deformedPos = position + (normal * noise * uDistortion);
        gl_Position = projectionMatrix * modelViewMatrix * vec4(deformedPos, 1.0);
    }
  `,
  // --- FRAGMENT SHADER (Updated with Structural Uniforms) ---
  `
    uniform float uTime;
    uniform float uIntensity;
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    
    // New Structural Uniforms
    uniform float uNoiseDensity;
    uniform float uWarpStrength;
    uniform float uGlowPower;
    uniform float uContrast;
    
    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vNormal;

    float random (in vec2 st) {
        return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
    }

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
        float frequency = 0.0;
        for (int i = 0; i < 5; i++) {
            value += amplitude * noise(st);
            st *= 2.0;
            amplitude *= 0.5;
        }
        return value;
    }

    void main() {
        // USE UNIFORM: Noise Density
        vec2 uv = vUv * uNoiseDensity;
        
        // 1. Domain Warping
        vec2 q = vec2(0.);
        q.x = fbm(uv + 0.1 * uTime);
        q.y = fbm(uv + vec2(1.0));

        vec2 r = vec2(0.);
        // USE UNIFORM: Warp Strength
        r.x = fbm(uv + uWarpStrength * q + vec2(1.7, 9.2) + 0.15 * uTime);
        r.y = fbm(uv + uWarpStrength * q + vec2(8.3, 2.8) + 0.126 * uTime);

        float f = fbm(uv + r);

        // 2. Base Color Mixing
        vec3 color = mix(uColorA, uColorB, clamp((f*f)*4.0, 0.0, 1.0));
        color = mix(color, uColorC, clamp(length(q), 0.0, 1.0));
        
        // 3. Dark Theme Colored Glow
        // USE UNIFORM: Glow Power
        float glowIntensity = pow(f, uGlowPower) * 4.0;
        vec3 coloredGlow = uColorA * glowIntensity;
        
        vec3 finalColor = (color * uIntensity) + coloredGlow;

        // 4. Final Darkening Contrast
        // USE UNIFORM: Contrast (Controls the dark falloff)
        // smoothstep(edge0, edge1, x) -> higher uContrast means edge0 is higher, so more black
        finalColor *= smoothstep(uContrast - 0.4, uContrast + 0.4, f);

        gl_FragColor = vec4(finalColor, 1.0);
    }
  `
);

extend({ DistortedFluidMaterial });

// Props passed here will now update the shader in real-time
const DistortedFluidSphere = (props) => {
  const materialRef = useRef();

  useFrame(({ clock }) => {
    if (materialRef.current) {
      materialRef.current.uTime = clock.getElapsedTime();
    }
  });

  return (
    <Sphere args={[1.5, 128, 128]}>
      <distortedFluidMaterial 
        ref={materialRef} 
        uNoiseDensity={3.0} // Scale of the pattern (try 1.0 to 10.0)
        uWarpStrength={1.0} // How swirly it is (try 0.0 to 5.0)
        uGlowPower={3.0}    // Thinness of veins (higher = thinner/sharper)
        uContrast={0.1}     // Darkness (try 0.5 for very dark, -0.5 for brighter)
        {...props} 
      />
    </Sphere>
  );
};

export default DistortedFluidSphere;