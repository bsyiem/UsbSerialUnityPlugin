package com.bsyiem.serialcommunicationplugin;

import android.app.Activity;
import android.app.Fragment;
import android.content.Context;
import android.hardware.usb.UsbDevice;
import android.hardware.usb.UsbDeviceConnection;
import android.hardware.usb.UsbManager;
import android.widget.Toast;

import com.felhr.usbserial.UsbSerialDevice;
import com.unity3d.player.UnityPlayer;

import java.util.Map;

public class UsbSerialManager extends Fragment {

    public static UsbSerialManager INSTANCE;
    public static final String TAG = "USB_SERIAL_MANAGER";
    protected String gameObjName;

    UsbManager usbManager;

    UsbDevice device;
    UsbDeviceConnection deviceConnection;

//    UsbSerialDevice serial;

    public static void instantiate(String gameObjName){
        INSTANCE = new UsbSerialManager();
        INSTANCE.gameObjName = gameObjName;
        UnityPlayer.currentActivity.getFragmentManager().beginTransaction().add(INSTANCE,UsbSerialManager.TAG).commit();
    }

    public void createUsbSerialDevice(){
        int retry = 5;
        while(this.usbManager == null && retry > 0) {
            this.setUpUsbManager();
//            Toast.makeText(this.getContext(),"Setting up USB Manager: Retry "+retry,Toast.LENGTH_LONG).show();
            retry--;
        }

        if(this.usbManager == null){
            Toast.makeText(this.getContext(),"Error Setting up USB Manager",Toast.LENGTH_LONG).show();
            return;
        }

        this.printDevices();
    }


    public boolean setUpUsbManager(){
        this.usbManager = (UsbManager) this.getActivity().getSystemService(Context.USB_SERVICE);
        return this.usbManager==null?false:true;
    }

    public void printDevices(){
        Toast.makeText(this.getContext(),String.valueOf(this.usbManager.getDeviceList().size()),Toast.LENGTH_LONG).show();
//        for(Map.Entry<String,UsbDevice> device: this.usbManager.getDeviceList().entrySet()){
//            Toast.makeText(this.getContext(),device.getValue().getVendorId(),Toast.LENGTH_LONG).show();
//        }
    }

    public void showText(String text){
        Toast.makeText(this.getContext(),text,Toast.LENGTH_LONG).show();
    }
}
