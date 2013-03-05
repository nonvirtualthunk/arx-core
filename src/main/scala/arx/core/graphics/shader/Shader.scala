package arx.core.graphics.shader

import org.lwjgl.BufferUtils
import org.lwjgl.opengl._
import ARBShaderObjects._
import arx.core.vec.{Vec4f, Vec3f, Vec2f}
import collection.mutable.{Stack, SynchronizedQueue, HashMap}
import arx.core.graphics.GL
import java.io.InputStream
import arx.core.{Noto, Application}
import scala.Application

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 9/26/11
 * Time: 9:09 PM
 * Created by nonvirtualthunk
 */

class Shader {
	var vertexShader: Int = 0
	var fragmentShader: Int = 0
	var shaderObject: Int = 0
	val uniformLocations = HashMap[String, Int]()
	var onBind: (Shader) => Unit = (s: Shader) => {}
	var deferred = new SynchronizedQueue[() => Unit]
	var onNextBind = new SynchronizedQueue[() => Unit]
	var name = "Default Shader Name"
	var uniformProviders : List[TUniformProvider] = Nil

	val attributeMap : HashMap[String,Int] = new HashMap[String,Int]()
	val uniformStacks = new HashMap[String,Stack[Any]]()

	def withUniformProvider ( up : TUniformProvider ) = { uniformProviders ::= up ; this }

	def bind() {
		if ( shaderObject == 0 || ! isBound ) {
			while ( deferred.nonEmpty ) {
				val dfunc = deferred.dequeue()
				dfunc()
			}

			GL.bindShader(shaderObject)

			while ( onNextBind.nonEmpty ) {
				(onNextBind.dequeue())()
			}
			onBind(this)
			for ( up <- uniformProviders ) {
				up.setUniforms(this)
			}
		}
	}

	def isBound = GL.activeShader == shaderObject

	def doWhileBound ( stmt : => Unit ) {
		if ( ! GL.disabled ) {
			if ( isBound && Application.isOpenGLThread ) {
				stmt
			} else {
				onNextBind.enqueue( () => stmt )
			}
		}
	}

	def loadFromStreams(vertexStream : InputStream, fragmentStream: InputStream): Shader = {
		if (!Application.isOpenGLThread) {
			deferred.enqueue( () => loadFromStreams(vertexStream,fragmentStream) )
		}
		else {
			uniformLocations.clear()
			attributeMap.clear()

			if (vertexStream != null) {
				loadShader(vertexStream, isVertexShader = true)
			}
			if (fragmentStream != null) {
				loadShader(fragmentStream, isVertexShader = false)
			}

//			if ( shaderObject == 0 ) {
				shaderObject = glCreateProgramObjectARB()
//			}

			glAttachObjectARB(shaderObject, vertexShader)
			glAttachObjectARB(shaderObject, fragmentShader)

			for ( (name,index) <- attributeMap ) {
				bindAttribLocation(index,name)
			}

			glLinkProgramARB(shaderObject)
			glValidateProgramARB(shaderObject)
			getInfo(shaderObject)
		}

		this
	}
	/** This will load the shader and then close the stream, since it may deffer the actual
	  * loading, do not close the stream yourself
	  *
	  * @param stream
	  * @param isVertexShader
	  */
	def loadShader(stream: InputStream, isVertexShader: Boolean) {
		if (!Application.isOpenGLThread) {
			deferred.enqueue( () => loadShader(stream, isVertexShader) )
		}
		else {
			val shader =
				if (isVertexShader) {
//					if ( vertexShader == 0 ) {
						vertexShader = glCreateShaderObjectARB(ARBVertexShader.GL_VERTEX_SHADER_ARB)
//					}
					vertexShader
				}
				else {
//					if ( fragmentShader == 0 ) {
						fragmentShader = glCreateShaderObjectARB(ARBFragmentShader.GL_FRAGMENT_SHADER_ARB)
//					}
					fragmentShader
				}


			val source = scala.io.Source.fromInputStream(stream)

			val shaderSource = new StringBuffer
			var i = 0
			for ( line <- source.getLines() ) {
				val tline = line.trim
				if ( tline.startsWith("attribute") ){
					//drop the semicolon, split on whitespace
					val sections = tline.dropRight(1).split(" ")
					attributeMap(sections(2)) = i
					i += 1
				}
				shaderSource.append(tline).append("\n")
			}

			source.close()
			stream.close()

			glShaderSourceARB(shader, shaderSource)
			glCompileShaderARB(shader)

			if (!getInfo(shader)) {
				println("ERROR, following source failed to compile :")
				println(shaderSource)

			}

		}
	}



	def getInfo(id: Int): Boolean = {
		val ibuf = BufferUtils.createIntBuffer(1)
		glGetObjectParameterARB(id, GL_OBJECT_INFO_LOG_LENGTH_ARB, ibuf)

		val logLength = ibuf.get
		if (logLength > 1) {
			val bbuf = BufferUtils.createByteBuffer(logLength)
			ibuf.flip()
			glGetInfoLogARB(id, ibuf, bbuf)
			val infoBytes = new Array[Byte](logLength)
			bbuf.get(infoBytes)
			println("Info log: " + new String(infoBytes))
			false
		}
		true
	}



	def pushUniform(uniformName: String, f: Float ) {
		if ( ! Application.isOpenGLThread || ! isBound ) { throw new IllegalStateException("Pushing a uniform makes no sense if not on the opengl thread, or not bound") }
		val stack = uniformStacks.getOrElseUpdate(uniformName,{ val s = new Stack[Any]();s.push(f);s})
		stack.push(f)


		val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
		if (location != -1) {
			glUniform1fARB(location, f)
		}

	}
	def popUniform(uniformName: String ) {
		if ( ! Application.isOpenGLThread || ! isBound ) { throw new IllegalStateException("Popping a uniform makes no sense if not on the opengl thread, or not bound") }
		val stack = uniformStacks.getOrElse(uniformName,new Stack())
		stack.pop()
		stack.top match {
			case f : Float => setUniform(uniformName,f)
			case i : Int => setUniform(uniformName,i)
			case v : Vec2f => setUniform(uniformName,v)
			case v : Vec3f => setUniform(uniformName,v)
			case v : Vec4f => setUniform(uniformName,v)
			case _ => Noto.error("Illegal type with pop uniform")
		}
	}
	def setUniform(uniformName: String, f: Float ) { setUniform(uniformName,f,false) }
	def setUniform(uniformName: String, f: Float, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != f ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(f)

				val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
				if (location != -1) {
					glUniform1fARB(location, f)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, i: Int) { setUniform(uniformName,i,false) }
	def setUniform(uniformName: String, i: Int, tolerateAbsence: Boolean) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != i ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(i)

				val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
				if (location != -1) {
					glUniform1iARB(location, i)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: Vec2f ) { setUniform(uniformName,v,false); }
	def setUniform(uniformName: String, v: Vec2f, tolerateAbsence: Boolean) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || ( stack.top != v ) ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
				if (location != -1) {
					glUniform2fARB(location, v.x, v.y)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: Vec3f ) { setUniform(uniformName,v,false); }
	def setUniform(uniformName: String, v: Vec3f, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != v ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
				if (location != -1) {
					glUniform3fARB(location, v.x, v.y, v.z)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: Vec4f ) { setUniform(uniformName,v,false) }
	def setUniform(uniformName: String, v: Vec4f, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != v ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, glGetUniformLocationARB(shaderObject, uniformName))
				if (location != -1) {
					glUniform4fARB(location, v.r, v.g, v.b, v.a)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniformUntyped(s: String, value: Any) {
		value match {
			case f : Float => setUniform(s,f)
			case i : Int => setUniform(s,i)
			case v : Vec2f => setUniform(s,v)
			case v3 : Vec3f => setUniform(s,v3)
			case v4 : Vec4f => setUniform(s,v4)
			case _ => Noto.warn("Shader, setUniform " + s + " , unknown type : " + value)
		}
	}

	def bindAttribLocation (i: Int,name : String) {
		GL20.glBindAttribLocation(shaderObject,i,name)
		Noto.finest("Binding attribute \"" + name + "\" to index " + i + " on shader " + shaderObject + " - " + this.name)
	}
}

object Shader {
	var boundShader : Option[Shader] = None
	def unbind() {
		GL.unbindShader()
	}
}

trait TUniformProvider {
	def setUniforms (shader : Shader)
}