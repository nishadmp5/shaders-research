import { useFrame } from "@react-three/fiber";
import { useMemo, useRef } from "react";
import * as THREE from "three";

// 1. THE VERTEX SHADER (The Architect)
// Its job here is simple:
// 1. Calculate screen position (gl_Position)
// 2. Pass the UV coordinates to the fragment shader (vUv)
const vertexShader = `
  varying vec2 vUv;

  void main() {
    vUv = uv; 
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
  }
`;

// 2. THE FRAGMENT SHADER (The Artist)
// Its job: Color the pixels based on the data received.
const fragmentShader = `
varying vec2 vUv;

void main() {
   
    vec2 gridUv = vUv * 3.0;

    gridUv = fract(gridUv);

    gridUv = gridUv - 0.5;

    float dist = length(gridUv);

    float circle = step(dist,0.25);

    gl_FragColor = vec4(vec3(circle),1.0);
}
`;

const ShaderPlane = () => {
  // Reference to the material so we can talk to it
  const materialRef = useRef();

  // 2. DEFINE THE UNIFORMS
  // We use useMemo so this object is stable
  const uniforms = useMemo(
    () => ({
      uTime: { value: 0 }, // Start time at 0
    }),
    [],
  );

  useFrame((state) => {
    const { clock } = state;
    // Update the "value" of uTime on our material
    if (materialRef.current) {
      materialRef.current.uniforms.uTime.value = clock.getElapsedTime();
    }
  });

  return (
    <mesh>
      {/* A simple flat plane, 2x2 units */}
      <planeGeometry args={[2, 2]} />

      {/* The raw shader material */}
      <shaderMaterial
        ref={materialRef}
        vertexShader={vertexShader}
        fragmentShader={fragmentShader}
        uniforms={uniforms}
      />
    </mesh>
  );
};

export default ShaderPlane;
