#include <Wire.h>
 
#define SLAVE_ADDRESS 0x04
int number = 0;
int state = 0;

void setup() {
  pinMode(13, OUTPUT);
  // initialize i2c as slave
  Wire.begin(SLAVE_ADDRESS);
  // define callbacks for i2c communication
  Wire.onReceive(receiveData);
  Wire.onRequest(sendData);
}

void loop() {
  // put your main code here, to run repeatedly:
  delay(100);
}

void receiveData(int byteCount){ 
  int s = 0;
  while(Wire.available()) {
    s = Wire.read();
  }
  
  if(s){
      digitalWrite(13, HIGH);
    }  else {
     digitalWrite(13, LOW); 
  }

}

void sendData(){
  char data[] = { 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F };
  Wire.write(data, 6);
}
