/*
 * @date    14.03.2015
 * @author  Alexander Rüedlinger <a.rueedlinger@gmail.com>
 *
 *
 * Skeleton sketch for a I2C slave device.
 * 
 * Commands:
 * - 0XA0 : sends a word on the I2C bus whose data contains the measured humidity value.
 * - 0XB0 : sends a word on the I2C bus whose data contains the measured temperature value. 
 *
 */

 
#include <Wire.h>
 
#define SLAVE_ADDRESS 0x04

uint8_t state = 0x00;

/*
 * Fake registers.
 *
 *
 */
 
 

void setup() {
  pinMode(13, OUTPUT);
  Serial.begin(9600);
  // initialize i2c as slave
  Wire.begin(SLAVE_ADDRESS);
  // define callbacks for i2c communication
  Wire.onReceive(receiveData);
  Wire.onRequest(sendData);
}

long c = 0;
void loop() {
  
  humidity = dht.readHumidity();
  temperature = dht.readTemperature();
  if(c > 100) {
    Serial.print("temperature: ");
    Serial.print(temperature);
    Serial.print(" ");
    Serial.print("humidity: ");
    Serial.print(humidity);
    Serial.println("");
    c = 0;
  }
  c++;
  delay(100);
}

void receiveData(int byteCount){ 
  while(Wire.available()) {
    state = Wire.read();
  }
  
  if(state){
      digitalWrite(13, HIGH);
    } else {
     digitalWrite(13, LOW); 
  }
}

void sendTemperatureWord() {
  float tmp = temperature;
  int8_t tmp_i = (int8_t) tmp;
  int8_t tmp_f = (tmp - tmp_i) * 100;
  char data[] = { tmp_i, tmp_f };
  Wire.write(data, 2);
}

void sendHumidityWord() {
  float hum = humidity;
  int8_t hum_i = (int8_t) hum;
  int8_t hum_f = (hum - hum_i) * 100;
  char data[] = { hum_i, hum_f };
  Wire.write(data, 2);
}

void sendData(){
  if(state == 0xA0) {
   sendHumidityWord();
  }
  
  if(state == 0xB0) {
   sendTemperatureWord(); 
  }
  
  state = 0x00;
}
