import React from 'react'
import { Canvas } from '@react-three/fiber'
import ShaderPlane from '../components/ShaderPlane'

const CanvasSetup = () => {
  return (
     <div className=' bg-amber-200 h-screen w-full'>
      <Canvas camera={{ position: [0, 0, 3] }}>
        <ShaderPlane />
      </Canvas>
    </div>
  )
}

export default CanvasSetup