package arx.core.vec;
import java.io.Externalizable;
import com.esotericsoftware.kryo.KryoSerializable;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.Kryo;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec4i implements Externalizable, KryoSerializable {
	protected int ri;
	protected int gi;
	protected int bi;
	protected int ai;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec4i(){}
	public InternVec4i(int ra, int ga, int ba, int aa) {
		ri = ra;
		gi = ga;
		bi = ba;
		ai = aa;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeInt(ri);

		out.writeInt(gi);

		out.writeInt(bi);

		out.writeInt(ai);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		ri = in.readInt();
		gi = in.readInt();
		bi = in.readInt();
		ai = in.readInt();
	}
	@Override
	public void write(Kryo kryo,Output out) {
		out.writeInt(ri);

		out.writeInt(gi);

		out.writeInt(bi);

		out.writeInt(ai);

	}

	@Override
	public void read(Kryo kryo,Input in) {
		ri = in.readInt();
		gi = in.readInt();
		bi = in.readInt();
		ai = in.readInt();
	}
}
