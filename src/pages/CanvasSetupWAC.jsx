import { OrbitControls } from "@react-three/drei";
import { Canvas } from "@react-three/fiber";
import FluidBlob from "../components/FluidBlob";
import DistortedFluidSphere from "../components/DistortedFluidSphere";
import GenAi from "../GenAI/GenAi";

const CanvasSetupWAC = () => {
  return (
    <div>
      <div style={{ width: "100vw", height: "100vh", background: "#050505" }}>
        <Canvas camera={{ position: [0, 0, 3] }}>
          {/* <DistortedFluidSphere/> */}
          {/* <FluidBlob/> */}
          <OrbitControls />
          <GenAi/>
        </Canvas>
      </div>
    </div>
  );
};

export default CanvasSetupWAC;
