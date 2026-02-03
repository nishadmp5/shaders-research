import { Sphere, shaderMaterial } from "@react-three/drei";
import { extend, useFrame } from "@react-three/fiber";
import { useRef } from "react";
import * as THREE from "three";
import { useControls } from "leva";

const FluidMaterial = shaderMaterial(
  {
    uTime: 0,
    uIntensity: 0.05,
    uColorA: new THREE.Color("#DA4ADB"),
    uColorB: new THREE.Color("#1B3684"),
    uColorC: new THREE.Color("#420F41"),
    uMainColor: new THREE.Color("#000000"), // Default base color
    uMinWidth: 0.5,
    uMaxWidth: 1.0,
  },
  // Vertex Shader (Unchanged)
  `
    varying vec2 vUv;
    void main() {
      vUv = uv;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  // Fragment Shader
  `
    precision mediump float;
    
    uniform float uTime;
    uniform float uIntensity;
    uniform vec3 uColorA;
    uniform vec3 uColorB;
    uniform vec3 uColorC;
    uniform vec3 uMainColor;
    uniform float uMinWidth;
    uniform float uMaxWidth;
    
    varying vec2 vUv;

    // Pseudo-random generator function
    float random(float seed) {
      return fract(sin(seed * 12.9898) * 43758.5453);
    }

    void main() {
      vec2 uv = vUv * 2.0 - 1.0;
      
      float i = 0.0;
      float s = 0.0;
      float d = 0.0;
      vec3 p = vec3(0.0);
      
      // Initialize color with the Main Sphere Color (dimmed background)
      vec3 col = uMainColor * 0.1;

      for(float j = 0.0; j < 32.0; j++) {
          i += 1.0;
          
          p = vec3(uv * d, d - 12.0);
          
          // Twist/Distortion
          p += sin(0.4 * uTime + 2.0 * p.yxz + i) * 0.5;
          
          // --- RANDOM WIDTH LOGIC ---
          // 1. Generate a random value (0.0 to 1.0) based on loop index 'i'
          float rnd = random(i);
          
          // 2. Mix between Min and Max width based on that random value
          float currentWidth = mix(uMinWidth, uMaxWidth, rnd);
          
          // 3. Apply width to distance field
          // The formula uses (1.0 / width) because in raymarching:
          // Large multiplier = Fast change = Thin/Sharp line
          // Small multiplier = Slow change = Thick/Soft line
          // We clamp to avoid division by zero
          float widthFactor = 1.0 / max(currentWidth, 0.001);

          s = 0.01 + widthFactor * 0.1 * abs(length(p) - 8.0);
          d += s;
          
          float phase = d + uTime * 2.0;
          
          vec3 tint = mix(uColorA, uColorB, 0.5 + 0.5 * cos(phase));
          tint = mix(tint, uColorC, 0.5 + 0.5 * sin(phase * 0.7)); 
          
          col += tint / s;
      }

      col *= uIntensity; 
      col = col / (1.0 + col); 

      gl_FragColor = vec4(col, 1.0);
    }
  `
);

extend({ FluidMaterial });

const FluidSphere = () => {
  const materialRef = useRef();

  const { colorA, colorB, colorC, mainColor, intensity, minWidth, maxWidth } = useControls({
    // Colors
    mainColor: { value: "#050505", label: "Main Sphere Color" }, // Dark background by default
    colorA: { value: "#DA4ADB", label: "Thread Color A" },
    colorB: { value: "#1B3684", label: "Thread Color B" },
    colorC: { value: "#420F41", label: "Thread Color C" },
    
    // Width Controls
    minWidth: { value: 0.5, min: 0.1, max: 5.0, step: 0.1, label: "Min Width" },
    maxWidth: { value: 2.0, min: 0.1, max: 5.0, step: 0.1, label: "Max Width" },
    
    // General
    intensity: { value: 0.02, min: 0.001, max: 0.1, step: 0.001, label: "Glow Intensity" },
  });

  useFrame(({ clock }) => {
    if (materialRef.current) {
      materialRef.current.uTime = clock.getElapsedTime();
    }
  });

  return (
    <Sphere args={[1.5, 64, 64]}>
      <fluidMaterial
        ref={materialRef}
        uColorA={colorA}
        uColorB={colorB}
        uColorC={colorC}
        uMainColor={mainColor}
        uMinWidth={minWidth}
        uMaxWidth={maxWidth}
        uIntensity={intensity}
      />
    </Sphere>
  );
};

export default FluidSphere;