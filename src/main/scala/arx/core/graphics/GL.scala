package arx.core.graphics

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:10 PM
 * Created by nonvirtualthunk
 */

import collection.mutable.{Stack, HashMap, SynchronizedQueue}
import data.{TextureBlock, VBO}
import org.lwjgl.opengl.{GL15, GL11, GL13}
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.ARBShaderObjects._
import scala.Some
import ref.SoftReference
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import arx.core.traits.TUpdatableEntity
import arx.core.datastructures.Rect
import arx.core.Application

object GL extends TUpdatableEntity {
	Application.registerForRenderThreadUpdate(this)

	val vbosToDestroy = new SynchronizedQueue[Int]()
	val texturesToDestroy = new SynchronizedQueue[Int]()

	var disabled = false

	var viewport = Rect(0,0,1440,900)

	var boundTexture = Array(0,0,0,0,0,0,0,0,0,0,0)
	var activeTextureNumber = 0

	def bindTexture ( textureName : Int ) {
		bindTexture(0,textureName)
	}
	def bindTexture ( textureNumber : Int , textureName : Int ) {
		if ( ! disabled ) {
			if ( textureName != boundTexture(textureNumber) ) {
				if ( activeTextureNumber != textureNumber ) {
					GL13.glActiveTexture(GL_TEXTURE0 + textureNumber)
					activeTextureNumber = textureNumber
				}
				glBindTexture(GL_TEXTURE_2D,textureName)
				boundTexture(textureNumber) = textureName
			}
		}
	}
	def loadIdentity () {
		if ( ! disabled ) { glLoadIdentity() }
	}

	var activeShader : Int = 0

	def bindShader ( shaderProgram : Int ) {
		if ( activeShader != shaderProgram ) {
			activeShader = shaderProgram
			glUseProgramObjectARB(shaderProgram)
		}
	}
	def unbindShader () { bindShader(0) }


	def destroyVBO ( vbo : VBO ) {
		vbosToDestroy.enqueue(vbo.points.name)
		vbosToDestroy.enqueue(vbo.indices.name)
	}

	def destroyTexture ( texture : TextureBlock ) {
		texturesToDestroy.enqueue(texture.textureID)
	}

	val glStates = new HashMap[Int,Stack[Boolean]]
	def glPushState(setting:Int,truth:Boolean) {
		if ( ! disabled ) {
			val stack = glStates.getOrElseUpdate(setting,{ val s = new Stack[Boolean]; s.push(false); s })
			val current = stack.top
			stack.push(truth)
			if ( current != truth ) {
				if ( truth ) { GL11.glEnable(setting) } else { GL11.glDisable(setting) }
			}
		}
	}
	def glPopState (setting:Int) {
		if ( ! disabled ) {
			val stack = glStates.getOrElse(setting,{ val s = new Stack[Boolean]; s.push(false); s })
			val current = stack.top
			stack.pop()
			if ( current != stack.top ) { if ( stack.top ) { GL11.glEnable(setting) } else { GL11.glDisable(setting) } }
		}
	}
	def glSetState ( setting : Int , enable : Boolean ) {
		if ( ! disabled ) {
			val stack = glStates.getOrElseUpdate(setting,{ val s = new Stack[Boolean]; s.push(! enable); s })
			val current = stack.top
			if ( current != enable ) {
				stack.pop()
				stack.push(enable)
				if ( enable ) { GL11.glEnable(setting) } else { GL11.glDisable(setting) }
			}
		}
	}

	var curDepthFunc = -1
	def glSetDepthFunc ( func : Int ) {
		if ( ! disabled ) {
			if ( curDepthFunc != func ) {
				GL11.glDepthFunc(func)
				curDepthFunc = func
			}
		}
	}

	var curAlphaFunc = -1
	var curAlphaRef = -1.0f
	def glSetAlphaFunc ( func : Int , ref : Float ) {
		if ( func != curAlphaFunc || ref != curAlphaRef ) {
			curAlphaFunc = func
			curAlphaRef = ref
			glAlphaFunc(func,ref)
		}
	}

	var cullFace = -1
	def glSetCullFace ( face : Int ) {
		if ( ! disabled ) {
			if ( cullFace != face ) {
				GL11.glCullFace(face)
				cullFace = face
			}
		}
	}

	var tick = 0;
	def update(f: Float) {
		if ( ! disabled ) {
			tick += 1
			while ( vbosToDestroy.nonEmpty ) {
				val vid = vbosToDestroy.dequeue()
				GL15.glDeleteBuffers(vid)
			}
			while ( texturesToDestroy.nonEmpty ) {
				val tid = texturesToDestroy.dequeue()
				GL11.glDeleteTextures(tid)
			}
		}
	}


//	var bufferCache = new ArrayBuffer[SoftReference[ByteBuffer]]()
	val wasteLimit = 0.15f

	var bufCache = Map[Int,List[SoftReference[ByteBuffer]]]()

	def makeNewBuffer ( size : Int ) = BufferUtils.createByteBuffer(size)

	val bumper = BufferUtils.createByteBuffer(1)

	def getBumperBuffer : ByteBuffer = bumper

	var bytesActive : Long = 0

	def createByteBuffer ( size : Int ) : ByteBuffer = {
		val ret = synchronized {
			bufCache.get(size) match {
				case Some(list) =>
					list match {
						case ref :: tail =>
							ref.get match {
								case Some(deref) => {
									bufCache += size -> tail
									deref
								}
								case None => {
									bufCache += size -> tail
									makeNewBuffer(size)
								}
							}
						case Nil =>
							bufCache -= size
							makeNewBuffer(size)
					}
				case None => makeNewBuffer(size)
			}
		}
		locker synchronized {
			bytesActive += ret.capacity()
//			Noto.info("Bytes active increased to : " + bytesActive);
		}
		ret

//		synchronized {
//			var i = 0;
//			while ( i < bufferCache.size ) {
//				val bufRef = bufferCache(i)
//				if ( bufRef != null ) {
//					val buf = bufRef()
//					if ( buf != null ) {
//						val cap = buf.capacity()
//						if ( cap >= size ){
//							if ( cap <= size * (1.0f + wasteLimit) ) {
//								bufferCache(i) = null
//								return buf;
//							} else {
//								//Everything that follows will be larger, just go ahead and create it
//								return makeNewBuffer(size)
//							}
//						}
//					}
//				}
//				i += 1
//			}
//
//			makeNewBuffer(size)
//		}
	}
	val locker = new Object()

	def freeByteBuffer ( buffer : ByteBuffer ) {
		if ( ! (buffer eq bumper) ) {
//			synchronized {
//				var insertBefore = bufferCache.size
//				var i = 0;
//				val bcs = bufferCache.size
//				while ( i < bcs ) {
//					val br = bufferCache(i)
//					if ( br != null ) {
//						val b = br()
//						if ( b != null ) {
//							if ( b.capacity() >= buffer.capacity() ) { insertBefore = i; i = bcs }
//						}
//					}
//					i += 1
//				}
			locker synchronized {
				bytesActive -= buffer.capacity()
//				Noto.info("Bytes active decreased to : " + bytesActive);
			}

			synchronized {
				buffer.position(0)
				buffer.limit(buffer.capacity())
	//				bufferCache.insert(insertBefore,new SoftReference(buffer))
				bufCache += buffer.capacity -> (new SoftReference(buffer) :: bufCache.getOrElse(buffer.capacity,Nil))
			}
//			}
		}
	}


	var elementsDrawnThisFrame : Long = 0.toLong
}