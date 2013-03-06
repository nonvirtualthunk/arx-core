package arx.core.serialization

import java.io.{ObjectInput, ObjectOutput}
import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/11
 * Time: 12:21 PM
 * Created by nonvirtualthunk
 */

class ArxOutputStream(val stream: ObjectOutput ) {
	def writeArrayBuffer[T] ( list : ArrayBuffer[T] , func : (T) => Unit ) {
		stream.writeInt(list.size);
		list.foreach ( func(_) )
	}
	def writeList[T] ( list : List[T] , func : (T) => Unit ) {
		stream.writeInt(list.size);
		list.foreach ( func(_) )
	}
	def write ( o : AnyRef ) { stream.writeObject(o) }
	def write ( i : Int ) { stream.writeInt(i) }
	def write ( f : Float ) { stream.writeFloat(f) }
	def write ( f : Long ) { stream.writeLong(f) }
	def write ( f : Boolean ) { stream.writeBoolean(f) }
	def writeUTF8 ( s : String ) { stream.writeUTF(s) }

	def writeChar ( i : Char ) { stream.writeChar(i) }
	def writeInt ( i : Int ) { stream.writeInt(i) }
	def writeFloat ( f : Float ) { stream.writeFloat(f) }
	def writeLong ( f : Long ) { stream.writeLong(f) }
	def writeShort ( s : Int ) { stream.writeShort(s) }
	def writeByte ( s : Int ) { stream.writeByte(s) }
	def writeBoolean ( f : Boolean ) { stream.writeBoolean(f) }

}

object ArxOutputStream {
	implicit def outputStream2ArxOutputStream ( stream : ObjectOutput ) : ArxOutputStream = new ArxOutputStream(stream)
}

class ArxInputStream(val stream: ObjectInput ) {

	def readAs[T] : T = { stream.readObject.asInstanceOf[T] }
	def read[T] : T = { stream.readObject.asInstanceOf[T] }

	class ImplicitReader(val ais: ArxInputStream) {}
	object ImplicitReader {
		implicit def impRead[T] (ir: ImplicitReader) : T = { ir.ais.readAs[T] }
	}
	def readImplicitly = new ImplicitReader(this)

	def readArrayBuffer[T] ( func : () => T ) : ArrayBuffer[T] = {
		val numElements = stream.readInt
		val ret = new ArrayBuffer[T](numElements)
		for ( i <- 0 until numElements ) {
			ret.append(func())
		}
		ret
	}
	def readList[T] ( func : () => T ) : List[T] = {
		val numElements = stream.readInt
		var ret = List[T]()
		for ( i <- 0 until numElements ) {
			ret ::= func()
		}
		ret.reverse
	}
	def readInt = stream.readInt
	def readLong = stream.readLong
	def readFloat = stream.readFloat
	def readBoolean = stream.readBoolean
	def readChar = stream.readChar
	def readShort = stream.readShort
	def readByte = stream.readByte
	def readUTF8 = stream.readUTF
}

object ArxInputStream {
	implicit def inputStream2ArxInputStream ( stream : ObjectInput ) : ArxInputStream = new ArxInputStream(stream)
}