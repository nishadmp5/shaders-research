import { Sphere, shaderMaterial } from "@react-three/drei";
import { extend, useFrame } from "@react-three/fiber";
import { useRef } from "react";
import * as THREE from "three";
import { useControls } from "leva";

// --- 1. THE HYBRID MATERIAL ---
const HybridFluidMaterial = shaderMaterial(
  {
    // Common Uniforms
    uTime: 0,
    uTransition: 0.0, // 0.0 = Fluid Style, 1.0 = Bubble Style
    uSpeed: 0.6,
    uDistortion: 0.4,
    uFrequency: 1.5,
    uIntensity: 1.0,

    // Style A (Fluid) specific uniforms
    uNoiseDensity: 2.0,
    uWarpStrength: 1.0,
    uContrast: 0.5,
    uMinWidth: 0.1,
    uMaxWidth: 1.0,
    uRimPower: 8.0,
    uRimStrength: 1.0,
    uInnerDarkness: 0.8,
    uRimColor: new THREE.Color("#8AAFFF"),
    uRimWanderSpeed: 1.5,
    uRimWanderJitter: 0.3,

    // Shared Colors (Used by both styles differently)
    uColorA: new THREE.Color("#E048D7"),
    uColorB: new THREE.Color("#5FCCFE"),
    uColorC: new THREE.Color("#521554"),
  },
  // --- VERTEX SHADER (Your Provided "Target" Logic + Compatibility fixes) ---
  `
    uniform float uTime;
    uniform float uDistortion;
    uniform float uSpeed;
    uniform float uFrequency;

    varying vec2 vUv;
    varying vec3 vViewNormal; // Renamed from vNormal for consistency
    varying vec3 vViewPosition;
    varying float vNoise;

    // --- Simplex 3D Noise ---
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

    float getDisplacement(vec3 position) {
        return snoise(vec3(position * uFrequency + uTime * uSpeed));
    }

    void main() {
        vUv = uv;
        
        // 1. Base Displacement
        float noise = getDisplacement(position);
        vNoise = noise;
        vec3 deformedPos = position + (normal * noise * uDistortion);

        // 2. Finite Difference Normals
        float epsilon = 0.01; 
        vec3 tangent = normalize(cross(normal, vec3(0.0, 1.0, 0.0)));
        if (length(tangent) < 0.001) tangent = normalize(cross(normal, vec3(0.0, 0.0, 1.0)));
        vec3 bitangent = normalize(cross(normal, tangent));
        
        vec3 neighborA = position + tangent * epsilon;
        vec3 neighborB = position + bitangent * epsilon;
        vec3 deformedA = neighborA + (normal * getDisplacement(neighborA) * uDistortion);
        vec3 deformedB = neighborB + (normal * getDisplacement(neighborB) * uDistortion);
        
        vec3 va = deformedA - deformedPos;
        vec3 vb = deformedB - deformedPos;
        vec3 computedNormal = normalize(cross(va, vb));

        // 3. Output
        vec4 modelViewPosition = modelViewMatrix * vec4(deformedPos, 1.0);
        vViewPosition = -modelViewPosition.xyz;
        
        // Use normalMatrix to convert to view space
        vViewNormal = normalMatrix * computedNormal; 
        
        gl_Position = projectionMatrix * modelViewPosition;
    }
  `,
  // --- FRAGMENT SHADER (Hybrid) ---
  `
    uniform float uTime;
    uniform float uTransition; // 0->1
    
    // Shared Uniforms
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    uniform float uIntensity;

    // Style A Uniforms
    uniform float uNoiseDensity;
    uniform float uWarpStrength;
    uniform float uContrast;
    uniform float uMinWidth;
    uniform float uMaxWidth;
    uniform float uRimPower;
    uniform float uRimStrength;
    uniform float uInnerDarkness;
    uniform vec3 uRimColor;
    uniform float uRimWanderSpeed;
    uniform float uRimWanderJitter;

    varying vec2 vUv;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;
    varying float vNoise;

    // --- Style A Helpers (FBM/Random) ---
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

    // --- LOGIC: STYLE A (Distorted Fluid) ---
    vec3 getFluidStyle() {
        vec2 uv = vUv * uNoiseDensity;
        
        vec2 q = vec2(0.);
        q.x = fbm(uv + 0.1 * uTime);
        q.y = fbm(uv + vec2(1.0));

        vec2 r = vec2(0.);
        r.x = fbm(uv + uWarpStrength * q + vec2(1.7, 9.2) + 0.15 * uTime);
        r.y = fbm(uv + uWarpStrength * q + vec2(8.3, 2.8) + 0.126 * uTime);

        float f = fbm(uv + r);

        // Colors
        vec3 color = mix(uColorA, uColorB, clamp((f*f)*4.0, 0.0, 1.0));
        color = mix(color, uColorC, clamp(length(q), 0.0, 1.0));
        
        // Glow/Lines
        float widthMap = smoothstep(0.0, 1.5, length(q)); 
        float currentWidth = mix(uMinWidth, uMaxWidth, widthMap);
        float sharpness = 1.0 / max(currentWidth, 0.001);
        float glowIntensity = pow(f, sharpness) * 4.0;
        vec3 fluidGlow = uColorA * glowIntensity;
        
        vec3 finalColor = (color * uIntensity) + fluidGlow;
        finalColor *= smoothstep(uContrast - 0.4, uContrast + 0.4, f);

        // Wandering Rim
        vec3 viewDir = normalize(vViewPosition);
        float wanderX = sin(uTime * uRimWanderSpeed) * uRimWanderJitter;
        float wanderY = cos(uTime * uRimWanderSpeed * 0.8) * uRimWanderJitter;
        vec3 wanderingNormal = normalize(vViewNormal + vec3(wanderX, wanderY, 0.0));

        float fresnel = 1.0 - dot(viewDir, wanderingNormal);
        finalColor *= clamp(mix(1.0 - uInnerDarkness, 1.0, fresnel), 0.0, 1.0);

        float rimParam = pow(fresnel, uRimPower);
        vec3 rimFinal = uRimColor * rimParam * uRimStrength;
        
        return finalColor + rimFinal;
    }

    // --- LOGIC: STYLE B (Bubble) ---
    vec3 getBubbleStyle() {
        vec3 normal = normalize(vViewNormal);
        vec3 viewDir = normalize(vViewPosition);

        // Fresnel
        float fresnel = dot(viewDir, normal);
        fresnel = clamp(1.0 - fresnel, 0.0, 1.0);
        
        // Sharper rim
        float rim = pow(fresnel, 2.5);

        // Smooth color blending using vertex noise
        vec3 colorMix = mix(uColorA, uColorB, vNoise * 0.5 + 0.5);

        vec3 finalColor = vec3(0.0);
        finalColor += colorMix * rim * 3.0; // Rim
        finalColor += colorMix * 0.1;       // Ambient center

        return finalColor;
    }

    void main() {
        // Calculate both visual styles
        vec3 styleA = getFluidStyle();
        vec3 styleB = getBubbleStyle();

        // Smoothly mix between them based on slider
        vec3 finalColor = mix(styleA, styleB, uTransition);

        gl_FragColor = vec4(finalColor, 1.0);
    }
  `
);

extend({ HybridFluidMaterial });

const TransitionSphere = (props) => {
  const materialRef = useRef();

  const { 
    transition, // THE MAIN SLIDER
    distort, frequency, speed,
    minWidth, maxWidth, 
    noiseDensity, warpStrength, contrast, intensity, 
    rimPower, rimStrength, innerDarkness, rimColor,
    colorA, colorB, colorC,
    rimWanderSpeed, rimWanderJitter 
  } = useControls("Transition Sphere", {
    // TRANSITION CONTROL
    transition: { value: 0.0, min: 0.0, max: 1.0, label: "MORPH (0=Fluid, 1=Bubble)" },

    // Shape
    distort: { value: 0.4, min: 0.0, max: 1.0 },
    frequency: { value: 1.5, min: 0.0, max: 5.0 },
    speed: { value: 0.6, min: 0.0, max: 2.0 },
    
    // Fluid Style Controls
    minWidth: { value: 0.1, min: 0.01, max: 2.0 },
    maxWidth: { value: 0.5, min: 0.01, max: 2.0 },
    noiseDensity: { value: 2.0, min: 0.1, max: 5.0 },
    warpStrength: { value: 1.0, min: 0.0, max: 5.0 },
    contrast: { value: 0.5, min: 0.0, max: 1.0 },
    intensity: { value: 1.0, min: 0.0, max: 5.0 },
    
    // Lighting
    rimPower: { value: 8.0, min: 0.5, max: 8.0 },
    rimStrength: { value: 1.0, min: 0.0, max: 5.0 },
    innerDarkness: { value: 0.8, min: 0.0, max: 1.0 },
    rimColor: { value: "#8AAFFF" },
    rimWanderSpeed: { value: 1.5, min: 0.1, max: 3.0 },
    rimWanderJitter: { value: 0.3, min: 0.0, max: 1.0 },
    
    colorA: "#e048d7",
    colorB: "#5fccfe",
    colorC: "#521554",
  });

  useFrame(({ clock }) => {
    if (materialRef.current) {
      materialRef.current.uTime = clock.getElapsedTime();
    }
  });

  return (
    <Sphere args={[1.5, 256, 256]}>
      <hybridFluidMaterial 
        ref={materialRef} 
        
        // The Transition
        uTransition={transition}

        // Shared Shape
        uDistortion={distort}
        uFrequency={frequency}
        uSpeed={speed}
        
        // Style A Specifics
        uMinWidth={minWidth}
        uMaxWidth={maxWidth}
        uNoiseDensity={noiseDensity}
        uWarpStrength={warpStrength}
        uContrast={contrast}
        uIntensity={intensity}
        uRimPower={rimPower}
        uRimStrength={rimStrength}
        uInnerDarkness={innerDarkness}
        uRimColor={new THREE.Color(rimColor)}
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

export default TransitionSphere;