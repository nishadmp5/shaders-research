import { Sphere, shaderMaterial } from "@react-three/drei";
import { extend, useFrame } from "@react-three/fiber";
import React, { useRef, useMemo } from "react";
import * as THREE from "three";

// 1. Define shader outside component to avoid recompilation
const DistortedFluidMaterial = shaderMaterial(
  {
    uTime: 0,
    uSpeed: 0.7,
    uDistortion: 0.0,
    uFrequency: 3.0,
    uIntensity: 1.0,
    
    // Structural Uniforms
    uNoiseDensity: 2.0,
    uWarpStrength: 1.0,
    uContrast: 0.5,
    uMinWidth: 0.1,
    uMaxWidth: 1.0,
    uVeinDefinition: 0.6, 

    // Backlight Uniforms
    uRimPower: 8.0,       
    uRimStrength: 1.25,    
    uInnerDarkness: 0.8,  
    uRimColor: new THREE.Color("#8AAFFF"), 
    
    // Wandering Fresnel
    uRimWanderSpeed: 1.5,
    uRimWanderJitter: 0.3,
    
    // Color Uniforms
    uColorA: new THREE.Color("#E048D7"),
    uColorB: new THREE.Color("#5FCCFE"),
    uColorC: new THREE.Color("#521554"),
    uColorD: new THREE.Color("#000000"), 
  },
  // Vertex Shader
  `
    uniform float uTime;
    uniform float uDistortion;
    uniform float uSpeed;
    uniform float uFrequency;

    varying vec2 vUv;
    varying float vNoise;
    varying vec3 vViewNormal;
    varying vec3 vViewPosition;

    // Ashima Simplex Noise (Optimized for GLSL)
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
  // Fragment Shader
  `
    uniform float uTime;
    uniform float uSpeed;
    uniform float uIntensity;
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    uniform vec3 uColorD;
    
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
        float moveTime = uTime * uSpeed; 

        // Fluid Pattern
        vec2 q = vec2(0.);
        q.x = fbm(uv + 0.5 * moveTime); 
        q.y = fbm(uv + vec2(1.0));

        vec2 r = vec2(0.);
        r.x = fbm(uv + uWarpStrength * q + vec2(1.7, 9.2) + 0.4 * moveTime);
        r.y = fbm(uv + uWarpStrength * q + vec2(8.3, 2.8) + 0.3 * moveTime);

        float f = fbm(uv + r);

        // Fluid Colors
        vec3 color = mix(uColorA, uColorB, clamp((f*f)*4.0, 0.0, 1.0));
        
        // Controlled Black Veins
        float blackVeins = smoothstep(0.0, uVeinDefinition, abs(f - 0.5) * 3.0);
        color = mix(color, uColorD, blackVeins);

        // Deep background color
        color = mix(color, uColorC, clamp(length(q), 0.0, 1.0));
        
        float widthMap = smoothstep(0.0, 1.5, length(q)); 
        float currentWidth = mix(uMinWidth, uMaxWidth, widthMap);
        float sharpness = 1.0 / max(currentWidth, 0.001);

        float glowIntensity = pow(f, sharpness) * 4.0;
        vec3 fluidGlow = uColorA * glowIntensity;
        
        vec3 finalColor = (color * uIntensity) + fluidGlow;
        finalColor *= smoothstep(uContrast - 0.4, uContrast + 0.4, f);

        // Backlighting Logic
        vec3 viewDir = normalize(vViewPosition);
        
        float wanderX = sin(uTime * uRimWanderSpeed) * uRimWanderJitter;
        float wanderY = cos(uTime * uRimWanderSpeed * 0.8) * uRimWanderJitter;
        vec3 wanderOffset = vec3(wanderX, wanderY, 0.0);
        
        vec3 wanderingNormal = normalize(vViewNormal + wanderOffset);
        float fresnel = 1.0 - dot(viewDir, wanderingNormal);
        
        finalColor *= clamp(mix(1.0 - uInnerDarkness, 1.0, fresnel), 0.0, 1.0);

        float rimParam = pow(fresnel, uRimPower);
        vec3 rimFinal = uRimColor * rimParam * uRimStrength;
        
        gl_FragColor = vec4(finalColor + rimFinal, 1.0);
    }
  `
);

extend({ DistortedFluidMaterial });

// 2. Wrap in React.memo for performance in large component trees
const DistortedFluidSphere = React.memo((props) => {
  const materialRef = useRef();

  // 3. Memoize color instances to prevent garbage collection churn on every render
  const uniforms = useMemo(() => ({
    rimColor: new THREE.Color("#8AAFFF"),
    colorA: new THREE.Color("#e048d7"),
    colorB: new THREE.Color("#2a7ed5"),
    colorC: new THREE.Color("#521554"),
    colorD: new THREE.Color("#000000"),
  }), []);

  useFrame(({ clock }) => {
    if (materialRef.current) {
      materialRef.current.uTime = clock.getElapsedTime();
    }
  });

  return (
    <Sphere args={[1.5, 128, 128]}>
      {/* 4. Pass stable references. R3F will now strictly update uniforms only if they change */}
      <distortedFluidMaterial 
        ref={materialRef} 
        
        // Uniforms
        uMinWidth={0.1}
        uMaxWidth={0.5}
        uNoiseDensity={5.0}
        uWarpStrength={4.0}
        uContrast={0.5}
        uIntensity={3}
        uSpeed={0.7}
        uVeinDefinition={0.75}
        
        uRimPower={8.0}
        uRimStrength={1.25}
        uInnerDarkness={0.9}
        uRimColor={uniforms.rimColor}
        
        uRimWanderSpeed={1.5}
        uRimWanderJitter={0.3}
        
        uColorA={uniforms.colorA}
        uColorB={uniforms.colorB}
        uColorC={uniforms.colorC}
        uColorD={uniforms.colorD}
        
        {...props} 
      />
    </Sphere>
  );
});

export default DistortedFluidSphere;