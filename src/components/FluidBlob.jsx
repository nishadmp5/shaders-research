import { Sphere } from "@react-three/drei";
import { useFrame } from "@react-three/fiber";
import { useMemo, useRef } from "react";
import * as THREE from "three";

// --- GLSL SHADERS ---

const vertexShader = `
  uniform float uTime;
  uniform float uDistortion;
  uniform float uSpeed;
  uniform float uFrequency;

  varying vec2 vUv;
  varying vec3 vNormal;
  varying vec3 vViewPosition;
  varying float vNoise;

  // <--- Insert GLSL Noise Function Here (Simplex 3D Noise) --->
  // Source: https://github.com/stegu/webgl-noise
  vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
  vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
  vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
  vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }

  float snoise(vec3 v) {
    const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
    const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
    vec3 i  = floor(v + dot(v, C.yyy) );
    vec3 x0 = v - i + dot(i, C.xxx) ;

    // Other corners
    vec3 g = step(x0.yzx, x0.xyz);
    vec3 l = 1.0 - g;
    vec3 i1 = min( g.xyz, l.zxy );
    vec3 i2 = max( g.xyz, l.zxy );

    vec3 x1 = x0 - i1 + C.xxx;
    vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
    vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

    // Permutations
    i = mod289(i);
    vec4 p = permute( permute( permute(
              i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
            + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
            + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients: 7x7 points over a square, mapped onto an octahedron.
    // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
    float n_ = 0.142857142857; // 1.0/7.0
    vec3  ns = n_ * D.wyz - D.xzx;

    vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

    vec4 x_ = floor(j * ns.z);
    vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

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

    //Normalise gradients
    vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
    p0 *= norm.x;
    p1 *= norm.y;
    p2 *= norm.z;
    p3 *= norm.w;

    // Mix final noise value
    vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
    m = m * m;
    return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), dot(p2,x2), dot(p3,x3) ) );
  }
  // <--- End Noise Function --->

  // Helper: Calculate displacement for a specific point
  float getDisplacement(vec3 position) {
      return snoise(vec3(position * uFrequency + uTime * uSpeed));
  }

  void main() {
    vUv = uv;
    
    // 1. Calculate Base Displacement
    float noise = getDisplacement(position);
    vNoise = noise;
    
    vec3 deformedPos = position + (normal * noise * uDistortion);

    // 2. Recalculate Normals (Finite Difference Method)
    // This makes the light "catch" the ripples correctly.
    // We sample two nearby points, displace them, and calculate the cross product.
    
    float epsilon = 0.01; // Tiny offset
    
    // Create two tangent vectors orthogonal to the normal
    vec3 tangent = normalize(cross(normal, vec3(0.0, 1.0, 0.0)));
    if (length(tangent) < 0.001) tangent = normalize(cross(normal, vec3(0.0, 0.0, 1.0))); // Safety for poles
    vec3 bitangent = normalize(cross(normal, tangent));
    
    // Get positions of neighbors
    vec3 neighborA = position + tangent * epsilon;
    vec3 neighborB = position + bitangent * epsilon;
    
    // Displace neighbors (using the same noise function!)
    vec3 deformedA = neighborA + (normal * getDisplacement(neighborA) * uDistortion);
    vec3 deformedB = neighborB + (normal * getDisplacement(neighborB) * uDistortion);
    
    // Compute new normal based on the slope between these points
    vec3 va = deformedA - deformedPos;
    vec3 vb = deformedB - deformedPos;
    vec3 computedNormal = normalize(cross(va, vb));

    // 3. Final Outputs
    vec4 modelViewPosition = modelViewMatrix * vec4(deformedPos, 1.0);
    vViewPosition = -modelViewPosition.xyz;
    
    // IMPORTANT: Use the computed normal instead of the original mesh normal
    vNormal = normalMatrix * computedNormal;
    
    gl_Position = projectionMatrix * modelViewPosition;
  }
`;

const fragmentShader = `
  uniform vec3 uColorA;
  uniform vec3 uColorB;
  
  varying vec2 vUv;
  varying vec3 vNormal;
  varying vec3 vViewPosition;
  varying float vNoise;

  void main() {
    vec3 normal = normalize(vNormal);
    vec3 viewDir = normalize(vViewPosition);

    // Fresnel
    float fresnel = dot(viewDir, normal);
    fresnel = clamp(1.0 - fresnel, 0.0, 1.0);
    
    // Sharper rim for the "Bubble" look
    float rim = pow(fresnel, 4.0);

    // Smooth color blending
    // Using vNoise to vary color subtly across the surface
    vec3 colorMix = mix(uColorA, uColorB, vNoise * 0.5 + 0.5);

    // Composition
    vec3 finalColor = vec3(0.0);
    
    // Add Rim Glow (High intensity)
    finalColor += colorMix * rim * 3.0;

    // Add subtle ambient surface color (so it's not totally black in center)
    // Multiplied by small factor (0.1) to keep the center dark like the video
    finalColor += colorMix * 0.1;

    gl_FragColor = vec4(finalColor, 1.0);
  }
`;

const FluidBlob = () => {
  const meshRef = useRef();
  
  const uniforms = useMemo(
    () => ({
      uTime: { value: 0 },
      uSpeed: { value: 0.2 },      // Slowed down for "viscous" liquid feel
      uDistortion: { value: 0.05 }, // Reduced to maintain sphere shape
      uFrequency: { value: 3.0 },  // Lower frequency = larger, smoother blobs
      uColorA: { value: new THREE.Color("#CA33C0") },
      uColorB: { value: new THREE.Color("#0055ff") },
    }),
    []
  );

  useFrame(({ clock }) => {
    if (meshRef.current) {
      meshRef.current.material.uniforms.uTime.value = clock.getElapsedTime();
      
      // Gentle rotation adds to the "floating" effect
      meshRef.current.rotation.y += 0.002;
      meshRef.current.rotation.z += 0.001;
    }
  });

  return (
    <Sphere args={[1.5, 256, 256]} ref={meshRef}>
      <shaderMaterial
        vertexShader={vertexShader}
        fragmentShader={fragmentShader}
        uniforms={uniforms}
      />
    </Sphere>
  );
};

export default FluidBlob;