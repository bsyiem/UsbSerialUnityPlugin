package com.bsyiem.serialcommunicationplugin;

import android.app.Fragment;
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
    }

    public void openConnection(){
        if(physicaloid.open()){
            // read listener, When new data is received from Arduino add it to Text view
            physicaloid.addReadListener(new ReadLisener() {
                @Override
                public void onRead(int size) {
                    byte[] buf = new byte[size];
                    physicaloid.read(buf, size);
                    Toast.makeText(INSTANCE.getContext(),new String(buf),Toast.LENGTH_LONG).show();
                }
            });
        }else {
            //Error while connecting
            Toast.makeText(INSTANCE.getContext(), "Cannot open", Toast.LENGTH_LONG).show();
        }
    }
}
