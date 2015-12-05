package com.heatonresearch.aifh.util;

import com.heatonresearch.aifh.AIFHError;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

public class FileUtil {
    public static void downloadFile(String url, File path) {
        try {
            URL website = new URL(url);
            ReadableByteChannel rbc = Channels.newChannel(website.openStream());
            FileOutputStream fos = new FileOutputStream(path);
            fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
        } catch(IOException ex) {
            throw new AIFHError(ex);
        }
    }

}
