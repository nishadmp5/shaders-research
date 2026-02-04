import * as THREE from "three";
import { shaderMaterial } from "@react-three/drei";
import { extend } from "@react-three/fiber";

// ------------------------------------------------------------------
// GLSL UTILS (Moved outside to keep main shader clean)
// ------------------------------------------------------------------
const glslNoise = `
  // Optimized Ashima Simplex Noise
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
`;

// ------------------------------------------------------------------
// SHADER DEFINITION
// ------------------------------------------------------------------
export const FluidMaterial = shaderMaterial(
  {
    uTime: 0,
    uHover: 0.0,
    uPhase: 0,
    uDistortion: 0.01,
    uFrequency: 3.0,
    uIntensity: 3.0,
    uNoiseDensity: 5.0,
    uWarpStrength: 4.0,
    uContrast: 0.5,
    uMinWidth: 0.1,
    uMaxWidth: 0.5,
    uVeinDefinition: 0.75,
    uRimPower: 8.0,
    uRimStrength: 1.25,
    uInnerDarkness: 0.9,
    uRimColor: new THREE.Color("#8AAFFF"),
    uRimWanderSpeed: 1.5,
    uRimWanderJitter: 0.3,
    uColorA: new THREE.Color("#e048d7"),
    uColorB: new THREE.Color("#2a7ed5"),
    uColorC: new THREE.Color("#521554"),
    uColorD: new THREE.Color("#000000"),
    uBlobColorA: new THREE.Color("#CA33C0"),
    uBlobColorB: new THREE.Color("#0055ff"),
  },
  // Vertex Shader
  `
    uniform float uPhase;
    uniform float uFrequency;
    uniform float uDistortion;
    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;

    ${glslNoise}

    float getDisplacement(vec3 position) {
        return snoise(vec3(position * uFrequency + uPhase));
    }

    void main() {
        vUv = uv;
        float noise = getDisplacement(position);
        vNoise = noise;
        vec3 deformedPos = position + (normal * noise * uDistortion);

        // OPTIMIZATION: epsilon reduced for tighter derivative calculation
        float epsilon = 0.001; 
        
        // Tangent/Bitangent calculation
        // Note: This is expensive (3x noise calc). 
        // For lower-end devices, consider passing a pre-calculated varying normal 
        // and skipping the dynamic normal recalculation.
        vec3 tangent = normalize(cross(normal, vec3(0.0, 1.0, 1.0))); 
        vec3 bitangent = normalize(cross(normal, tangent));
        vec3 neighborA = position + tangent * epsilon;
        vec3 neighborB = position + bitangent * epsilon;
        
        vec3 deformedA = neighborA + (normal * getDisplacement(neighborA) * uDistortion);
        vec3 deformedB = neighborB + (normal * getDisplacement(neighborB) * uDistortion);
        
        vec3 va = deformedA - deformedPos;
        vec3 vb = deformedB - deformedPos;
        vec3 computedNormal = normalize(cross(va, vb));

        vec4 mvPosition = modelViewMatrix * vec4(deformedPos, 1.0);
        vViewPosition = -mvPosition.xyz;
        vViewNormal = normalMatrix * computedNormal;
        gl_Position = projectionMatrix * mvPosition;
    }
  `,
  // Fragment Shader
  `
    uniform float uTime;
    uniform float uHover;
    uniform float uPhase;
    uniform float uIntensity;
    uniform float uNoiseDensity;
    uniform float uWarpStrength;
    uniform float uContrast;
    uniform float uMinWidth;
    uniform float uMaxWidth;
    uniform float uVeinDefinition;
    uniform float uRimPower;
    uniform float uRimStrength;
    uniform float uInnerDarkness;
    uniform vec3 uRimColor;
    uniform float uRimWanderSpeed;
    uniform float uRimWanderJitter;
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    uniform vec3 uColorD;
    uniform vec3 uBlobColorA;
    uniform vec3 uBlobColorB;
    
    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;

    // Standard pseudo-randomness
    float random (in vec2 st) { return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123); }
    
    // Value noise 
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

    // Fractal Brownian Motion
    float fbm (in vec2 st) {
        float value = 0.0;
        float amplitude = 0.5;
        // OPTIMIZATION: Reduced iterations from 5 to 4. 
        // Visually similar, 20% faster loop.
        for (int i = 0; i < 4; i++) {
            value += amplitude * noise(st);
            st *= 2.0;
            amplitude *= 0.5;
        }
        return value;
    }

    vec3 getStateAColor(vec2 uv, vec3 viewDir, vec3 normal) {
        float moveTime = uPhase; 
        
        // Domain warping
        vec2 q = vec2(0.);
        q.x = fbm(uv + 0.5 * moveTime); 
        q.y = fbm(uv + vec2(1.0));
        
        vec2 r = vec2(0.);
        r.x = fbm(uv + uWarpStrength * q + vec2(1.7, 9.2) + 0.4 * moveTime);
        r.y = fbm(uv + uWarpStrength * q + vec2(8.3, 2.8) + 0.3 * moveTime);
        
        float f = fbm(uv + r);
        
        // Color Mixing
        vec3 color = mix(uColorA, uColorB, clamp((f*f)*4.0, 0.0, 1.0));
        float blackVeins = smoothstep(0.0, uVeinDefinition, abs(f - 0.5) * 3.0);
        color = mix(color, uColorD, blackVeins);
        color = mix(color, uColorC, clamp(length(q), 0.0, 1.0));
        
        // Glow & Definition
        float widthMap = smoothstep(0.0, 1.5, length(q)); 
        float currentWidth = mix(uMinWidth, uMaxWidth, widthMap);
        float sharpness = 1.0 / max(currentWidth, 0.001);
        float glowIntensity = pow(f, sharpness) * 4.0;
        vec3 fluidGlow = uColorA * glowIntensity;
        
        vec3 finalColor = (color * uIntensity) + fluidGlow;
        finalColor *= smoothstep(uContrast - 0.4, uContrast + 0.4, f);
        
        // Rim Lighting Logic
        float wanderX = sin(uTime * uRimWanderSpeed) * uRimWanderJitter;
        float wanderY = cos(uTime * uRimWanderSpeed * 0.8) * uRimWanderJitter;
        vec3 wanderingNormal = normalize(normal + vec3(wanderX, wanderY, 0.0));
        
        float fresnel = 1.0 - dot(viewDir, wanderingNormal);
        finalColor *= clamp(mix(1.0 - uInnerDarkness, 1.0, fresnel), 0.0, 1.0);
        
        float rimParam = pow(fresnel, uRimPower);
        vec3 rimFinal = uRimColor * rimParam * uRimStrength;
        
        return finalColor + rimFinal;
    }

    vec3 getStateBColor(vec3 viewDir, vec3 normal) {
        float fresnel = 1.0 - dot(viewDir, normal);
        fresnel = clamp(fresnel, 0.0, 1.0);
        float rim = pow(fresnel, 2.5);
        vec3 colorMix = mix(uBlobColorA, uBlobColorB, vNoise * 0.5 + 0.5);
        
        // Simplified mixing for State B
        return (colorMix * rim * 3.0) + (colorMix * 0.1);
    }

    void main() {
        // Mirror UVs for seamless noise effect
        vec2 seamlessUv = vec2(abs(vUv.x - 0.5) * 2.0, vUv.y);
        vec2 uv = seamlessUv * uNoiseDensity;
        
        vec3 viewDir = normalize(vViewPosition);
        vec3 normal = normalize(vViewNormal);
        
        vec3 colorA = getStateAColor(uv, viewDir, normal);
        
        // OPTIMIZATION: Only calculate State B if we are hovering (or transitioning)
        // This branching is usually bad in shaders, but if uHover is 0.0 constant, 
        // the GPU branch predictor might skip the calculation.
        vec3 colorB = vec3(0.0);
        if (uHover > 0.001) {
             colorB = getStateBColor(viewDir, normal);
             gl_FragColor = vec4(mix(colorA, colorB, uHover), 1.0);
        } else {
             gl_FragColor = vec4(colorA, 1.0);
        }
    }
  `
);

// Register the material with R3F
extend({ FluidMaterial });