import { Canvas } from "@react-three/fiber";
import React from "react";
import FluidBlob from "../components/FluidBlob";
import { OrbitControls } from "@react-three/drei";

const CanvasSetupWAC = () => {
  return (
    <div style={{ width: "100vw", height: "100vh", background: "#050505" }}>
      <Canvas camera={{ position: [0, 0, 3] }}>
        <FluidBlob />
        <OrbitControls />
      </Canvas>
    </div>
  );
};

export default CanvasSetupWAC;
