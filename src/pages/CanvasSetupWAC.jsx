import { Canvas } from "@react-three/fiber";
import DistortedFluidSphere from "../components/CombinedFluidSphere";
import FluidBlob from "../components/FluidBlob";
import { OrbitControls } from "@react-three/drei";
import FluidSphere from "../components/FluidSphere";

const CanvasSetupWAC = () => {
  return (
    <div>
      <div style={{ width: "100vw", height: "100vh", background: "#050505" }}>
        <Canvas camera={{ position: [0, 0, 3] }}>
          {/* <FluidSphere/> */}
          {/* <FluidBlob/> */}
          <OrbitControls />
          <DistortedFluidSphere />
        </Canvas>
      </div>
    </div>
  );
};

export default CanvasSetupWAC;
