/*
special chars used when receiving:
# - terminate (switch off all leds)
$ - permission to light up virtual led
@ - reaction time recorded - switch off all leds
 */


package com.bsyiem.serialcommunicationplugin;

import android.Manifest;
import android.app.Fragment;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;
import android.widget.Toast;

import androidx.core.content.ContextCompat;

import com.physicaloid.lib.Physicaloid;
import com.physicaloid.lib.usb.driver.uart.ReadLisener;
import com.unity3d.player.UnityPlayer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class SerialCommunication extends Fragment {

    public static SerialCommunication INSTANCE;
    public static final String TAG = "SERIAL_MANAGER";
    protected String gameObjName;

    protected Physicaloid physicaloid;

    protected String fileName;

    RecorderRunnable myRunnable;

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
//        Toast.makeText(this.getContext(),"Set File Name:" + fileName, Toast.LENGTH_LONG).show();
    }

    public static void instantiate(String gameObjName){
        INSTANCE = new SerialCommunication();
        INSTANCE.gameObjName = gameObjName;
        UnityPlayer.currentActivity.getFragmentManager().beginTransaction().add(INSTANCE,SerialCommunication.TAG).commit();
    }

    public void createPhysicaloid(int baudRate){
        this.physicaloid = new Physicaloid(this.getContext());
        this.physicaloid.setBaudrate(baudRate);
//        Toast.makeText(this.getContext(),"physicaloid for baud rate: "+baudRate, Toast.LENGTH_LONG).show();
    }

    public void openConnection(){
        if(physicaloid.open()){
            Toast.makeText(INSTANCE.getContext(), "Connection Open", Toast.LENGTH_LONG).show();

            myRunnable = new RecorderRunnable();
            myRunnable.setContext(INSTANCE.getContext());
            myRunnable.setFileName(fileName);
            myRunnable.setGameObjName(this.gameObjName);
            myRunnable.setTAG(TAG);

            //for testing to display text
//            DisplayTextRunnable myRunnable = new DisplayTextRunnable();
//            myRunnable.setContext(INSTANCE.getContext());

            // read listener, When new data is received from Arduino
            physicaloid.addReadListener(new ReadLisener() {
                @Override
                public void onRead(int size) {
                    Log.d(TAG,"received info");
                    byte[] buf = new byte[size];
                    physicaloid.read(buf, size);

                    Log.d(TAG, new String(buf));

                    myRunnable.setText(new String(buf));

                    //for testing
//                    myRunnable.setText(new String(buf));

                    UnityPlayer.currentActivity.runOnUiThread(myRunnable);
//                    Toast.makeText(INSTANCE.getContext(),new String(buf),Toast.LENGTH_LONG).show();
                }
            });
        }else {
            //Error while connecting
            Toast.makeText(INSTANCE.getContext(), "Cannot Open Connection ", Toast.LENGTH_LONG).show();
//            UnityPlayer.UnitySendMessage(this.gameObjName,"HandleArduinoMessage", "#");
        }
    }

    public void writeToFile(String text){
        myRunnable.writeText(text);
    }

    public void closeFile(){
        myRunnable.closeFOS();
    }

    private void displayToast(String str){
        Toast.makeText(INSTANCE.getContext(), str, Toast.LENGTH_LONG).show();
    }

    public void closeConnection(){
        this.physicaloid.close();
    }

    public void sendData(String str){
        byte[] buf = str.getBytes();
        physicaloid.write(buf,buf.length);
    }
}

class RecorderRunnable implements Runnable{
    String text;
    Context context;
    String fileName;
    String gameObjName;
    String TAG;

    FileOutputStream fos = null;

    public void setContext(Context context) {
        this.context = context;
    }

    public void setText(String text){
        this.text = text;
    }

    public void setFileName(String fileName){
        this.fileName = fileName;
        try {
            fos = context.openFileOutput(fileName,Context.MODE_PRIVATE);
        } catch (FileNotFoundException e) {
            Toast.makeText(this.context, "Error OStream", Toast.LENGTH_LONG).show();
            e.printStackTrace();
        }
    }

    public void setGameObjName(String gameObjName){
        this.gameObjName = gameObjName;
    }

    public void setTAG(String TAG){
        this.TAG = TAG;
    }

    public void writeText(String text){
        if(fos!=null) {
            try {
                fos.write(text.getBytes());
            } catch (IOException e) {
                Log.d(TAG, "Error in writing");
                e.printStackTrace();
            }
        }
    }

    public void closeFOS(){
        if (fos != null) {
            try {
                fos.close();
            } catch (IOException e) {
                Toast.makeText(this.context, "Error Closing OStream", Toast.LENGTH_LONG).show();
                e.printStackTrace();
            }
        }
    }

    @Override
    public void run() {

        //"$" switch on LED
        if(this.text.equals("$")){
            UnityPlayer.UnitySendMessage(this.gameObjName,"HandleArduinoMessage","$");
        }else{
            // "#" program complete - end
            if (this.text.equals("#")) {
                Toast.makeText(this.context, "Terminating", Toast.LENGTH_LONG).show();

                //ask for the number of ball passes and store in file.
                //closeFOS();
                UnityPlayer.UnitySendMessage(this.gameObjName,"HandleArduinoMessage", "#");

            } else {
                try {
                    // received reaction time- switch off all leds
                    UnityPlayer.UnitySendMessage(this.gameObjName,"HandleArduinoMessage","@");
                    fos.write(text.getBytes());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

    }
}

class DisplayTextRunnable implements Runnable{

    String text;
    Context context;

    public void setContext(Context context) {
        this.context = context;
    }

    public void setText(String text){
        this.text = text;
    }

    @Override
    public void run() {
        Toast.makeText(this.context, this.text, Toast.LENGTH_LONG).show();
    }
}