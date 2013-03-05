package arx.core.vec;
import java.io.Externalizable;
import com.esotericsoftware.kryo.KryoSerializable;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.Kryo;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec4f implements Externalizable, KryoSerializable {
	protected float ri;
	protected float gi;
	protected float bi;
	protected float ai;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec4f(){}
	public InternVec4f(float ra, float ga, float ba, float aa) {
		ri = ra;
		gi = ga;
		bi = ba;
		ai = aa;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeFloat(ri);

		out.writeFloat(gi);

		out.writeFloat(bi);

		out.writeFloat(ai);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		ri = in.readFloat();
		gi = in.readFloat();
		bi = in.readFloat();
		ai = in.readFloat();
	}
	@Override
	public void write(Kryo kryo,Output out) {
		out.writeFloat(ri);

		out.writeFloat(gi);

		out.writeFloat(bi);

		out.writeFloat(ai);

	}

	@Override
	public void read(Kryo kryo,Input in) {
		ri = in.readFloat();
		gi = in.readFloat();
		bi = in.readFloat();
		ai = in.readFloat();
	}
}
