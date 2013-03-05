package arx.core.serialization

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/8/13
 * Time: 8:32 AM
 * Created by nonvirtualthunk
 */

import java.io.{ObjectOutput, ObjectInput, Externalizable}

trait TVersionedExternalizable extends Externalizable {
	protected def currentVersion : Int

	protected def writeExternalVersioned(out : ArxOutputStream)
	protected def readExternalVersioned(version : Int, in : ArxInputStream)

	def writeExternal(out: ObjectOutput) {
		out.writeShort(currentVersion)
		writeExternalVersioned(new ArxOutputStream(out))
	}
	def readExternal(in: ObjectInput) {
		val v = in.readShort
		readExternalVersioned(v,new ArxInputStream(in))
	}
}