import React, { Suspense } from "react";
import { Canvas } from "@react-three/fiber";
import { AdaptiveDpr, Preload } from "@react-three/drei";
import GenAiSphere from "../components/GenAiProduction/GenAiSphere";
import * as THREE from "three";

const CanvasSetupWAC = () => {
  return (
    <div style={{ width: "100%", height: "100vh", background: "#050505" }}>
      <Canvas
        camera={{ position: [0, 0, 5], fov: 45 }}
        // Optimization: Cap pixel ratio to 2 to save battery on mobile/retina
        dpr={[1, 2]} 
        // Optimization: Standardize color management
        gl={{ 
            toneMapping: THREE.ACESFilmicToneMapping,
            outputColorSpace: THREE.SRGBColorSpace,
            antialias: true,
            powerPreference: "high-performance",
        }}
      >
        <AdaptiveDpr  />

        <Suspense fallback={null}>
          <GenAiSphere />
        </Suspense>
        
        {/* Preload ensures shaders compile before first paint */}
        <Preload all />
      </Canvas>
    </div>
  );
};

export default CanvasSetupWAC;

   {/* <DistortedFluidSphere/> */}
          {/* <FluidBlob/> */}
          {/* <OrbitControls /> */}