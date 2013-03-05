package arx.core.graphics.view

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:28 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec3f
import org.lwjgl.util.glu.GLU

trait TCamera {
	def eye : ReadVec3f
	def eye_= ( v : ReadVec3f )

	def forward : ReadVec3f
	def forward_= ( v : ReadVec3f )

	def up : ReadVec3f
	def up_= ( v : ReadVec3f )

	def look () {
		val _eye = eye
		val _forward = forward.normalizeSafe
		val _up = up
		GLU.gluLookAt(_eye.x,_eye.y,_eye.z,_eye.x + _forward.x,_eye.y + _forward.y,_eye.z + _forward.z,_up.x,_up.y,_up.z)
	}
}