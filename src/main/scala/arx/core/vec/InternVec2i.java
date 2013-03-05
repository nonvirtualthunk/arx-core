package arx.core.vec;
import java.io.Externalizable;
import com.esotericsoftware.kryo.KryoSerializable;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.Kryo;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec2i implements Externalizable, KryoSerializable {
	protected int xi;
	protected int yi;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec2i(){}
	public InternVec2i(int xa, int ya) {
		xi = xa;
		yi = ya;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeInt(xi);

		out.writeInt(yi);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		xi = in.readInt();
		yi = in.readInt();
	}
	@Override
	public void write(Kryo kryo,Output out) {
		out.writeInt(xi);

		out.writeInt(yi);

	}

	@Override
	public void read(Kryo kryo,Input in) {
		xi = in.readInt();
		yi = in.readInt();
	}
}
