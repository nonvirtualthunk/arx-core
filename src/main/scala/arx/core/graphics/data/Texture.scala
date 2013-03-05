package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 1/25/13
 * Time: 6:46 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30
import arx.core.vec.ReadVec2i

class Texture {
	var name = 0
	var imageData : Image = Image.Sentinel
	var minFilter : Int = GL_NEAREST
	var magFilter : Int = GL_NEAREST
	var mipmap : Boolean = true
	var revision = 0

	def bind () {
		GL.bindTexture(name)
	}

	def commitTexture () {
		if ( name == 0 ) {
			name = glGenTextures()
		}

		GL.bindTexture(name)
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,minFilter)
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,magFilter)
		glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

		imageData.data.rewind()
		glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,imageData.width,imageData.height,0,GL_RGBA,GL_UNSIGNED_BYTE,imageData.data)
		if ( mipmap ) {
			GL30.glGenerateMipmap(GL_TEXTURE_2D)
		}
	}

	def commitSubTexture (offset : ReadVec2i,dims : ReadVec2i) {
		if ( name == 0 ) { commitTexture() }
		else {
			GL.bindTexture(name)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,minFilter)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,magFilter)
			glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

			imageData.data.rewind()
			glTexSubImage2D(GL_TEXTURE_2D,0,offset.x,offset.y,dims.x,dims.y,GL_RGBA,GL_UNSIGNED_BYTE,imageData.data)
			if ( mipmap ) {
				GL30.glGenerateMipmap(GL_TEXTURE_2D)
			}
		}
	}
}

object Texture {
	def fromImage (img : Image) = {
		val t = new Texture
		t.imageData = img
		t
	}
}