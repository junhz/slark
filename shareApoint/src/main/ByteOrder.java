package main;

import java.nio.ByteBuffer;

public enum ByteOrder {

	BIG_ENDIAN(java.nio.ByteOrder.BIG_ENDIAN),
	LITTLE_ENDIAN(java.nio.ByteOrder.LITTLE_ENDIAN)
	;
	private final java.nio.ByteOrder order;
	
	ByteOrder(java.nio.ByteOrder order) {
		this.order = order;
	}
	
	public ByteBuffer wrap(byte[] bytes) {
		ByteBuffer buffer = ByteBuffer.allocate(bytes.length).order(order).put(bytes);
		buffer.position(0);
		return buffer;
	}
	
	public ByteBuffer allocate(int i) {
		return ByteBuffer.allocate(i).order(order);
	}
	
}
