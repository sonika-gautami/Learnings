package leetcode.arrays;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;

public class Arrays2D {
    public static void main(String[] args) throws IOException {
        byte[] bytes = new FileInputStream("fileToRead").readAllBytes();

        Byte[] bytes_ = new Byte[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            bytes_[i] = bytes[i];
        }

        Arrays.stream(bytes_);
        //.collect(Collectors.reducing())

// acc, byte
        // 1 -> int 32 4 byte
        //  0 1 2 3 - int
        //  1000 0000  0000 0000  0000 0000  0000 0001

        // ((buffer[i+2]) << 24) + ((buffer[i+3]) << 16) + ((buffer[i+4]) << 8  + (buffer[i+5]);


    }

    class data {
        private int expectedSize;
        private byte[] bytesOfDataType;
    }
}
