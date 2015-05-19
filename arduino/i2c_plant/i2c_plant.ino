/*
 * @date    15.03.2015
 * @author  Alexander Rüedlinger <a.rueedlinger@gmail.com>
 *
 *
 * Skeleton sketch for a I2C slave device.
 * 
 * Commands:
 * - 0XA0 : sends a word on the I2C bus whose data contains the measured humidity value.
 *
 */

#include <Wire.h>

#define SLAVE_ADDRESS 0x05

#define MAX_SENSOR_VALUE 1023.0  // max value of the analog input pin 

uint8_t state = 0x00;

int sensor_pin = 2;        // sensor pin
int sensor_value = 0;     // analog pin input value
float humidity = 0;         // soil moisture in percentage
 

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
  
  sensor_value = analogRead(sensor_pin);    // read the value from the sensor
  humidity = (1.0 - ((float)sensor_value)/MAX_SENSOR_VALUE) * 100;
  if(c > 100) {
    Serial.print("humidity: ");
    Serial.print(humidity);
    Serial.println("");
    c = 0;
  }
  c++;
  delay(200);
}

void receiveData(int byteCount){ 
  int i;
  while(Wire.available()) {
   state = Wire.read();
  }
}

void sendHumidityWord() {
  sensor_value = analogRead(sensor_pin);    // read the value from the sensor
  humidity = (1.0 - ((float)sensor_value)/MAX_SENSOR_VALUE) * 100;
  float hum = humidity;
  int8_t hum_i = (int8_t) hum;
  int8_t hum_f = (hum - hum_i) * 100;
  char data[] = { hum_i, hum_f };
  Wire.write(data, 2);
}

void sendData(){
  sendHumidityWord();
  state = 0x00;
}
