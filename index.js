import * as THREE from 'three';
// import {GLTFLoader} from 'three/examples/jsm/loaders/GLTFLoader.js';
import * as BufferGeometryUtils from 'three/examples/jsm/utils/BufferGeometryUtils.js';
// import {renderer, camera, runtime, world, universe, physics, ui, rig, app, appManager, popovers} from 'app';
import metaversefile from 'metaversefile';
import Simplex from './simplex-noise.js';
import alea from './alea.js';
const {useFrame, useLocalPlayer, useCleanup, /*useUi,*/ usePhysics} = metaversefile;

export default () => {  
  const parcelSize = 16;
  const width = 10;
  const height = 10;
  const depth = 10;
  const streetSize = new THREE.Vector3(10, 1, 1000);

  const zeroVector = new THREE.Vector3(0, 0, 0);
  const zeroQuaternion = new THREE.Quaternion();
  const oneVector = new THREE.Vector3(1, 1, 1);
  const localVector = new THREE.Vector3();
  const localVector2 = new THREE.Vector3();
  const localVector3 = new THREE.Vector3();
  const localVector2D = new THREE.Vector2();
  const localQuaternion = new THREE.Quaternion();
  const localEuler = new THREE.Euler();
  const localMatrix = new THREE.Matrix4();

  const rootScene = new THREE.Object3D();

  class MultiSimplex {
    constructor(seed, octaves) {
      const simplexes = Array(octaves);
      for (let i = 0; i < octaves; i++) {
        simplexes[i] = new Simplex(seed + i);
      }
      this.simplexes = simplexes;
    }
    noise2D(x, z) {
      let result = 0;
      for (let i = 0; i < this.simplexes.length; i++) {
        const simplex = this.simplexes[i];
        result += simplex.noise2D(x * (2**i), z * (2**i));
      }
      // result /= this.simplexes.length;
      return result;
    }
  }

  const gridSimplex = new MultiSimplex('lol', 6);
  const gridSimplex2 = new MultiSimplex('lol2', 6);
  const streetMesh = (() => {
    const material = new THREE.ShaderMaterial({
      uniforms: {
        uTime: {
          type: 'f',
          value: 0,
        },
        /* uBeat: {
          type: 'f',
          value: 1,
        }, */
      },
      vertexShader: `\
        ${THREE.ShaderChunk.common}

        ${THREE.ShaderChunk.logdepthbuf_pars_vertex}
        attribute float y;
        attribute vec3 barycentric;
        varying float vUv;
        varying vec3 vBarycentric;
        varying vec3 vPosition;
        void main() {
          vUv = uv.x;
          vBarycentric = barycentric;
          vPosition = position;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
          ${THREE.ShaderChunk.logdepthbuf_vertex}
        }
      `,
      fragmentShader: `\
        // uniform float uBeat;
        varying vec3 vBarycentric;
        varying vec3 vPosition;
        uniform float uTime;
        ${THREE.ShaderChunk.logdepthbuf_pars_fragment}

        struct C{
            float d;
            int t;
        };
            
        float rand2(vec2 co){
            return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
        }

        float rand(vec3 v){
            return rand2(vec2(v.x+v.z,v.y+v.z));
        }

        int wait(float t){
            float period = 4.*3.141592/1.5;
            t = mod(t,period);
            if(t < period/2.){
                if(t < period/8.)return 0;
                if(t < period/4.)return 1;
                return int((t/period-1./4.)*40.)+2;
            }else{
                t-=period/2.;
                if(t < period/8.)return 10;
                if(t < period/4.)return 9;
                return 8-int((t/period-1./4.)*40.);
            }
            return 0;
        }

        float scal(float t){
            float period = 4.*3.141592/1.5;
            t = mod(t,period);
            float base = -1000.0;
            if(t < period/2.){
                if(t < period/8.)base=-1000.0;
                else if(t < period/4.)base=period/8.;
                else if(t<period*(1./4.+9./40.)){
                    int x = int((t/period-1./4.)*40.);
                  base = period*(1./4.+float(x)/40.);
                }
            }else{
                t -= period/2.;
                if(t < period/8.)base=-1000.0;
                else if(t < period/4.)base=period/8.;
                else if(t<period*(1./4.+9./40.)){
                    int x = int((t/period-1./4.)*40.);
                  base = period*(1./4.+float(x)/40.);
                }
            }
            return exp(-(t-base)*10.);
        }

        vec3 transform(vec3 p){
            float t = uTime+sin(uTime*1.5);
            p -= vec3(4,0,0);
            p *= mat3(cos(t),0,sin(t),0,1,0,-sin(t),0,cos(t));
            t *= 1.2;
            t += sin(uTime*0.5);
            p *= mat3(cos(t),sin(t),0,-sin(t),cos(t),0,0,0,1);
            return p;
        }

        float pattern(vec3 p, float section) {
          // p = transform(p);
          /* float s = 1.; // (0.7+scal(uTime)*0.08) / 0.7;
          p /= s;
          p /= 1.3;
          p += 0.5; */
          float d = 0.;
          // float t = uTime;
          vec3 e = vec3(section);
          for(int i=0;i<10;i++){
              // if(wait(t) <= i)break;
              float w = pow(2.,float(i));
              float f;

              f = rand(vec3(0,0,float(i))+e);
              if(p.x < f)e.x+=w;
              else e.x-=w;
              if(pow(max(0.,1.-abs(p.x-f)),90.)*1.5 > 0.5+float(i)/20.)d = 1.;

              f = rand(vec3(1,0,float(i))+e);
              if(p.y < f)e.y+=w;
              else e.y-=w;
              if(pow(max(0.,1.-abs(p.y-f)),90.)*1.5 > 0.5+float(i)/20.)d = 1.;

              f = rand(vec3(2,0,float(i))+e);
              if(p.z < f)e.z+=w;
              else e.z-=w;
              if(pow(max(0.,1.-abs(p.z-f)),90.)*1.5 > 0.5+float(i)/20.)d = 1.;
          }
          return d<1.?0.:1.;
        }

        const vec3 lineColor1 = vec3(${new THREE.Color(0x4fc3f7).toArray().join(', ')});
        // const vec3 lineColor2 = vec3(${new THREE.Color(0x9575cd).toArray().join(', ')});

        void main() {
          // vec3 c = mix(lineColor1, lineColor2, vPosition.y / 10.);
          vec3 c = lineColor1;
          vec3 p = mod(vec3(vPosition.x*0.99, vPosition.y, vPosition.z)/10. + 0.5, 1.);
          float section = floor(vPosition.z/10.);
          float f = pattern(p, section);
          gl_FragColor = vec4(c * (f > 0.5 ? 1. : 0.2) /* * uBeat */, 1.);
          gl_FragColor = sRGBToLinear(gl_FragColor);

          ${THREE.ShaderChunk.logdepthbuf_fragment}
        }
      `,
      side: THREE.DoubleSide,
    });
    const geometry = new THREE.BoxBufferGeometry(streetSize.x, streetSize.y, streetSize.z);
    const mesh = new THREE.Mesh(geometry, material);
    return mesh;
  })();

  rootScene.add(streetMesh);
  streetMesh.matrix.setPosition(0, -1/2, 0);
  streetMesh.matrixAutoUpdate = false;

  const stacksBoundingBox = new THREE.Box2(
    new THREE.Vector2(5, 0),
    new THREE.Vector2(105, 100),
  );
  const forestBoundingBox = new THREE.Box2(
    new THREE.Vector2(-40, -2),
    new THREE.Vector2(-5, 30),
  );
  const gridMesh = (() => {
    const geometry = (() => {
      const s = 300;
      // const maxManhattanDistance = localVector2D.set(0, 0).manhattanDistanceTo(localVector2D2.set(s/2, s/2));

      let geometry = new THREE.PlaneBufferGeometry(s, s, s, s)
        .applyMatrix4(new THREE.Matrix4().makeRotationFromQuaternion(new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3(0, 0, 1), new THREE.Vector3(0, 1, 0))));

      for (let i = 0; i < geometry.attributes.position.array.length; i += 3) {
        const x = geometry.attributes.position.array[i];
        const z = geometry.attributes.position.array[i+2];
        const d = Math.abs(x); 
        const f = Math.min(Math.max((d - 5) / 30, 0), 1)**2;
        const y = -0.01 +
          Math.min(Math.max(gridSimplex.noise2D(x/500, z/500) * f * 30, 0), 100) *
          Math.min(stacksBoundingBox.distanceToPoint(new THREE.Vector2(x, z)), 1) *
          Math.min(forestBoundingBox.distanceToPoint(new THREE.Vector2(x, z)), 1);
        // console.log('got distance', z, d/maxDistance);
        geometry.attributes.position.array[i+1] = y;
      }
      const dynamicPositionYs = new Float32Array(geometry.attributes.position.array.length/3);
      for (let i = 0; i < dynamicPositionYs.length; i += 3) {
        const x = geometry.attributes.position.array[i*3];
        const z = geometry.attributes.position.array[i*3+2];

        // const d = Math.abs(x); 
        // const f = Math.min(Math.max((d - 5) / 30, 0), 1)**2;

        const y = gridSimplex2.noise2D(x/500, z/500) * 3;
        dynamicPositionYs[i] = y;
        dynamicPositionYs[i+1] = y;
        dynamicPositionYs[i+2] = y;
      }
      geometry.setAttribute('dynamicPositionY', new THREE.BufferAttribute(dynamicPositionYs, 1));

      geometry = geometry.toNonIndexed();
      const barycentrics = new Float32Array(geometry.attributes.position.array.length);
      let barycentricIndex = 0;
      for (let i = 0; i < geometry.attributes.position.array.length; i += 9) {
        barycentrics[barycentricIndex++] = 1;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 1;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 1;
      }
      geometry.setAttribute('barycentric', new THREE.BufferAttribute(barycentrics, 3));

      return geometry;
    })();

    const material = new THREE.ShaderMaterial({
      uniforms: {
        /* uBeat: {
          type: 'f',
          value: 1,
        },
        uBeat2: {
          type: 'f',
          value: 0,
        }, */
      },
      vertexShader: `\
         ${THREE.ShaderChunk.common}

        attribute float y;
        attribute vec3 barycentric;
        attribute float dynamicPositionY;
        // uniform float uBeat2;
        varying float vUv;
        varying vec3 vBarycentric;
        varying vec3 vPosition;
        ${THREE.ShaderChunk.logdepthbuf_pars_vertex}

        void main() {
          vUv = uv.x;
          vBarycentric = barycentric;
          vPosition = position;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position /* + vec3(0., dynamicPositionY * uBeat2, 0.) */, 1.0);

          ${THREE.ShaderChunk.logdepthbuf_vertex}
        }
      `,
      fragmentShader: `\
        // uniform float uBeat;
        precision highp float;
        precision highp int;

        #define PI 3.1415926535897932384626433832795

        varying vec3 vBarycentric;
        varying vec3 vPosition;

        const vec3 lineColor1 = vec3(${new THREE.Color(0x66bb6a).toArray().join(', ')});
        const vec3 lineColor2 = vec3(${new THREE.Color(0x9575cd).toArray().join(', ')});

        ${THREE.ShaderChunk.logdepthbuf_pars_fragment}

        float edgeFactor(vec3 bary, float width) {
          // vec3 bary = vec3(vBC.x, vBC.y, 1.0 - vBC.x - vBC.y);
          vec3 d = fwidth(bary);
          vec3 a3 = smoothstep(d * (width - 0.5), d * (width + 0.5), bary);
          return min(min(a3.x, a3.y), a3.z);
        }

        void main() {
          vec3 c = mix(lineColor1, lineColor2, vPosition.y / 10.);
          // vec3 p = fwidth(vPosition);
          vec3 p = vPosition;
          float f = min(mod(p.x, 1.), mod(p.z, 1.));
          f = min(f, mod(1.-p.x, 1.));
          f = min(f, mod(1.-p.z, 1.));
          f *= 10.;
          float a = /*0.7 + */max(1. - f, 0.);
          if (a < 0.5) {
            discard;
          } else {
            gl_FragColor = vec4(c /* * uBeat */, a);
            gl_FragColor = sRGBToLinear(gl_FragColor);
          }

          ${THREE.ShaderChunk.logdepthbuf_fragment}
        }
      `,
      side: THREE.DoubleSide,
      transparent: true,
    });
    const mesh = new THREE.Mesh(geometry, material);
    return mesh;
  })();


  rootScene.add(gridMesh);
  gridMesh.position.set(0, -0.01, 0);
  gridMesh.updateMatrix();
  gridMesh.updateMatrixWorld();
  const particlesMesh = (() => {
    const numParticles = 30000;
    const s = 0.1;
    const spread = 20;
    const geometry = (() => {
      const geometry = new THREE.BufferGeometry();
      const positions = new Float32Array(1000 * 9);
      for (let i = 0; i < positions.length; i += 9) {
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).multiplyScalar(s).toArray(positions, i);
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).multiplyScalar(s).toArray(positions, i+3);
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).multiplyScalar(s).toArray(positions, i+6);
      }
      geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
      const barycentrics = new Float32Array(positions.length);
      let barycentricIndex = 0;
      for (let i = 0; i < geometry.attributes.position.array.length; i += 9) {
        barycentrics[barycentricIndex++] = 1;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 1;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 0;
        barycentrics[barycentricIndex++] = 1;
      }
      geometry.setAttribute('barycentric', new THREE.BufferAttribute(barycentrics, 3));
      const offset = new Float32Array(positions.length);
      for (let i = 0; i < geometry.attributes.position.array.length; i += 9) {
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).multiplyScalar(spread);
        localVector.toArray(offset, i);
        localVector.toArray(offset, i+3);
        localVector.toArray(offset, i+6);
      }
      geometry.setAttribute('offset', new THREE.BufferAttribute(offset, 3));
      const dynamicPositions = new Float32Array(positions.length);
      for (let i = 0; i < geometry.attributes.position.array.length; i += 9) {
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).multiplyScalar(5);
        localVector.toArray(dynamicPositions, i);
        localVector.toArray(dynamicPositions, i+3);
        localVector.toArray(dynamicPositions, i+6);
      }
      geometry.setAttribute('dynamicPosition', new THREE.BufferAttribute(dynamicPositions, 3));
      const dynamicRotations = new Float32Array(positions.length);
      for (let i = 0; i < geometry.attributes.position.array.length; i += 9) {
        localVector.set(Math.random(), Math.random(), Math.random()).subScalar(0.5).normalize();
        localVector.toArray(dynamicRotations, i);
        localVector.toArray(dynamicRotations, i+3);
        localVector.toArray(dynamicRotations, i+6);
      }
      geometry.setAttribute('dynamicRotation', new THREE.BufferAttribute(dynamicRotations, 3));
      const timeOffset = new Float32Array(positions.length/3);
      for (let i = 0; i < timeOffset.length; i += 3) {
        const r = Math.random();
        timeOffset[i] = r;
        timeOffset[i+1] = r;
        timeOffset[i+2] = r;
      }
      geometry.setAttribute('timeOffset', new THREE.BufferAttribute(timeOffset, 1));

      return geometry;
    })();

    const material = new THREE.ShaderMaterial({
      uniforms: {
        uColor: {
          type: 'f',
          value: 0,
        },
        /* uBeat: {
          type: 'f',
          value: 0,
        },
        uBeat2: {
          type: 'f',
          value: 0,
        }, */
        uTime: {
          type: 'f',
          value: 0,
        },
      },
      vertexShader: `\
          
        ${THREE.ShaderChunk.common}

        ${THREE.ShaderChunk.logdepthbuf_pars_vertex}

        vec3 applyQuaternion(vec3 v, vec4 q) { 
          return v + 2.0*cross(cross(v, q.xyz ) + q.w*v, q.xyz);
        }
        vec4 axisAngleToQuaternion(vec3 axis, float angle) {
          float half_angle = angle/2.;
          vec4 q;
          q.x = axis.x * sin(half_angle);
          q.y = axis.y * sin(half_angle);
          q.z = axis.z * sin(half_angle);
          q.w = cos(half_angle);
          return q;
        }
        vec4 mult(vec4 a, vec4 b) {
          return a * b;
        }
        vec3 applyAxisAngle(vec3 vector, vec3 axis, float angle) {
          return applyQuaternion(vector, axisAngleToQuaternion(axis, angle));
        }

        uniform float uTime;
        // uniform float uBeat2;
        // attribute float y;
        attribute vec3 offset;
        attribute vec3 barycentric;
        attribute vec3 dynamicPosition;
        attribute vec3 dynamicRotation;
        attribute float timeOffset;
        // varying float vUv;
        varying vec3 vBarycentric;
        // varying vec3 vPosition;
        void main() {
          // vUv = uv.x;
          vBarycentric = barycentric;
          // vPosition = position;
          vec3 p = position /* * (1. + uBeat2) */;
          vec3 o = offset + dynamicPosition * mod(timeOffset + uTime, 1.);
          o -= cameraPosition;
          o = mod(o, ${spread.toFixed(8)});
          o -= ${(spread/2).toFixed(8)};
          o += cameraPosition;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(applyAxisAngle(p, dynamicRotation, uTime * PI*2.) + o, 1.0);

          ${THREE.ShaderChunk.logdepthbuf_vertex}
        }
      `,
      fragmentShader: `\
        precision highp float;
        precision highp int;

        #define PI 3.1415926535897932384626433832795
        
        ${THREE.ShaderChunk.logdepthbuf_pars_fragment}

        uniform float uColor;
        // uniform float uBeat;
        varying vec3 vBarycentric;
        // varying vec3 vPosition;

        // const vec3 lineColor1 = vec3(${new THREE.Color(0x66bb6a).toArray().join(', ')});
        // const vec3 lineColor2 = vec3(${new THREE.Color(0x9575cd).toArray().join(', ')});

        float edgeFactor(vec3 bary, float width) {
          // vec3 bary = vec3(vBC.x, vBC.y, 1.0 - vBC.x - vBC.y);
          vec3 d = fwidth(bary);
          vec3 a3 = smoothstep(d * (width - 0.5), d * (width + 0.5), bary);
          return min(min(a3.x, a3.y), a3.z);
        }

        void main() {
          // vec3 c = mix(lineColor1, lineColor2, vPosition.y / 10.);
          vec3 c = vec3(uColor);
          float f = edgeFactor(vBarycentric, 1.);
          gl_FragColor = vec4(c /* * uBeat */, max(1. - f, 0.));
          // gl_FragColor = vec4(c, 1.);

          ${THREE.ShaderChunk.logdepthbuf_fragment}
        }
      `,
      side: THREE.DoubleSide,
      transparent: true,
    });
    const mesh = new THREE.Mesh(geometry, material);
    mesh.frustumCulled = false;
    return mesh;
  })();
  rootScene.add(particlesMesh);

  const physics = usePhysics();
  const floorPhysicsId = physics.addBoxGeometry(
    new THREE.Vector3(0, -1000, 0),
    streetMesh.quaternion,
    new THREE.Vector3(streetSize.z, 2000, streetSize.z).multiplyScalar(0.5),
    false
  );
  /* app.addEventListener('unload', () => {
    physics.removeGeometry(physicsId);
  }); */

  /* let beat = false;
  let beatReady = false;
  const listener = new THREE.AudioListener();
  rootScene.add(listener);
  const sound = new THREE.Audio(listener);
  new THREE.AudioLoader().load(`https://avaer.github.io/assets-private/mnleo.mp3`, function( buffer ) {
    sound.setBuffer(buffer);
    sound.setLoop(true);
    // sound.setVolume(0.5);
    // sound.play();
    beatReady = true;
  });
  const analyser = new THREE.AudioAnalyser(sound, 32);
  const _keydown = e => {
    switch (e.which) {
      case 77: { // M
        if (beatReady) {
          beat = !beat;
          if (beat) {
            sound.play();
          } else {
            sound.pause();
          }
        }
        break;
      }
    }
  };
  window.addEventListener('keydown', _keydown);
  app.addEventListener('unload', () => {
    window.removeEventListener('keydown', _keydown);
  }); */

  /* appManager.addEventListener('use', () => {
    universe.enterWorld({
      objects: [
        {
          position: [-3, 0, -10],
          contentId: 'https://avaer.github.io/shield/index.js',
        },
      ],
      extents: [
        portalMesh.boundingBox.min.toArray(),
        portalMesh.boundingBox.max.toArray(),
      ],
    });
    // rootScene.visible = !rootScene.visible;
  }); */

  /* app.addEventListener('activate', e => {
    console.log('got activate event', e, e.waitUntil);
  }); */

  useFrame(({timestamp}) => {
    // const transforms = rig.getRigTransforms();
    // const {position} = transforms[0];
    const {position} = useLocalPlayer();
    const f = 1 - Math.min(Math.max(-position.z / 30, -0.3), 0.3) / 0.3;

    streetMesh.material.uniforms.uTime.value = (timestamp%10000)/20;
    // floorMesh.material.uniforms.uAnimation.value = (now%2000)/2000;
    particlesMesh.material.uniforms.uColor.value = f;
    particlesMesh.material.uniforms.uTime.value = (timestamp%10000)/10000;

    /* if (beat) {
      const fd = analyser.getFrequencyData();
      const v = fd[4];
      const beatValue = Math.min(v/255, 1);
      const beatValue2 = Math.min(v/255, 1);
      streetMesh.material.uniforms.uBeat.value = beatValue;
      gridMesh.material.uniforms.uBeat.value = beatValue;
      gridMesh.material.uniforms.uBeat2.value = beatValue2;
      particlesMesh.material.uniforms.uBeat.value = beatValue;
      particlesMesh.material.uniforms.uBeat2.value = beatValue2;
    } else {
      streetMesh.material.uniforms.uBeat.value = 1;
      gridMesh.material.uniforms.uBeat.value = 1;
      gridMesh.material.uniforms.uBeat2.value = 0;
      particlesMesh.material.uniforms.uBeat.value = 1;
      particlesMesh.material.uniforms.uBeat2.value = 0;
    } */
  });
  
  useCleanup(() => {
    // console.log('clean up street');
    physics.removeGeometry(floorPhysicsId);
  });

  return rootScene;
}
