package com.bsyiem.serialcommunicationplugin;

import android.app.Fragment;
import android.content.Context;
import android.util.Log;
import android.widget.Toast;

import com.physicaloid.lib.Physicaloid;
import com.physicaloid.lib.usb.driver.uart.ReadLisener;
import com.unity3d.player.UnityPlayer;

public class SerialCommunication extends Fragment {

    public static SerialCommunication INSTANCE;
    public static final String TAG = "SERIAL_MANAGER";
    protected String gameObjName;

    protected Physicaloid physicaloid;

    public static void instantiate(String gameObjName){
        INSTANCE = new SerialCommunication();
        INSTANCE.gameObjName = gameObjName;
        UnityPlayer.currentActivity.getFragmentManager().beginTransaction().add(INSTANCE,SerialCommunication.TAG).commit();
    }

    public void createPhysicaloid(){
        this.physicaloid = new Physicaloid(this.getContext());
        this.physicaloid.setBaudrate(9600);
        Toast.makeText(this.getContext(),"physicaloid created", Toast.LENGTH_LONG).show();
        Log.d(TAG,"debug test");
    }

    public void openConnection(){
        if(physicaloid.open()){
            Log.d(TAG,"port open");
            // read listener, When new data is received from Arduino add it to Text view
            physicaloid.addReadListener(new ReadLisener() {
                @Override
                public void onRead(int size) {
                    Log.d(TAG,"received info");
                    byte[] buf = new byte[size];
                    physicaloid.read(buf, size);

                    DisplayTextRunnable myRunnable = new DisplayTextRunnable();
                    myRunnable.setContext(INSTANCE.getContext());
                    myRunnable.setText(new String(buf));

                    UnityPlayer.currentActivity.runOnUiThread(myRunnable);
//                    Toast.makeText(INSTANCE.getContext(),new String(buf),Toast.LENGTH_LONG).show();
                }
            });
        }else {
            //Error while connecting
            Toast.makeText(INSTANCE.getContext(), "Cannot open", Toast.LENGTH_LONG).show();
        }
    }

    private void displayToast(String str){
        Toast.makeText(INSTANCE.getContext(), str, Toast.LENGTH_LONG).show();
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