import React, { useRef, useMemo } from "react";
import * as THREE from "three";
import { useFrame } from "@react-three/fiber";
import { Sphere } from "@react-three/drei";
import { easing } from "maath";

// Import the material we just defined
import { FluidMaterial } from "./FluidMaterial";


const GenAiSphere = ({ radius = 1.5, segments = 256 }) => {
  const materialRef = useRef();
  const meshRef = useRef();
  const groupRef = useRef();
  
  // Track hover state with a Ref, NOT useState
  // This prevents the entire component from re-rendering when you mouse over
  const isHovered = useRef(false);

  // Memoize Colors once. 
  // We pass these via uniforms to avoid recreating them every frame.
  const uniforms = useMemo(() => ({
    uRimColor: new THREE.Color("#8AAFFF"),
    uColorA: new THREE.Color("#e048d7"),
    uColorB: new THREE.Color("#2a7ed5"),
    uColorC: new THREE.Color("#521554"),
    uColorD: new THREE.Color("#000000"),
    uBlobColorA: new THREE.Color("#CA33C0"),
    uBlobColorB: new THREE.Color("#0055ff"),
  }), []);

  useFrame((state, delta) => {
    // Safety check
    if (!materialRef.current || !groupRef.current) return;

    // 1. DAMPING VALUES (Smooth Transitions)
    // damp(currentValue, targetValue, speed, delta)
    // We update the uniforms directly on the GPU
    
    // Smoothly transition 'uHover' uniform from 0 to 1
    easing.damp(
        materialRef.current, 
        "uHover", 
        isHovered.current ? 1.0 : 0.0, 
        0.25, 
        delta
    );

    // Smoothly transition Distortion based on hover status
    const targetDistortion = isHovered.current ? 0.03 : 0.005;
    easing.damp(materialRef.current, "uDistortion", targetDistortion, 0.25, delta);

    // 2. TIME & PHASE
    materialRef.current.uTime = state.clock.getElapsedTime();
    // Calculate phase speed based on hover intensity (derived from the uniform we just damped)
    const speed = THREE.MathUtils.lerp(0.7, 1.0, materialRef.current.uHover);
    materialRef.current.uPhase += delta * speed;

    // 3. MESH ROTATION (Spin)
    meshRef.current.rotation.y += 0.002;

    // 4. MOUSE TILT (Interactive)
    // Using maath for smoother mouse follow than standard Lerp
    const { pointer } = state;
    const tiltStrength = 0.3; // Increased slightly for better feel

    easing.dampE(
        groupRef.current.rotation,
        [pointer.y * -tiltStrength, pointer.x * tiltStrength, 0],
        0.25, // smooth time
        delta
    );
  });

  return (
    <group ref={groupRef}>
      <Sphere
        ref={meshRef}
        args={[radius, segments, segments]}
        // Mutate Ref directly, avoid re-render
        onPointerOver={() => (isHovered.current = true)}
        onPointerOut={() => (isHovered.current = false)}
        frustumCulled={false}
      >
        <fluidMaterial
          ref={materialRef}
          key={FluidMaterial.key} // Helps R3F identify unique material instances
          transparent={false} // Solid objects render faster if false
          
          // Spread memoized uniforms
          {...uniforms}
          
          // Static props
          uIntensity={3.0}
          uNoiseDensity={5.0}
          uWarpStrength={4.0}
          uContrast={0.5}
          uMinWidth={0.1}
          uMaxWidth={0.5}
          uVeinDefinition={0.75}
          uRimPower={8.0}
          uRimStrength={1.25}
          uInnerDarkness={0.9}
          uRimWanderSpeed={1.5}
          uRimWanderJitter={0.3}
        />
      </Sphere>
    </group>
  );
};

export default GenAiSphere;