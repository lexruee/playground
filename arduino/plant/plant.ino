/*
 * @author    Alexander Rüedlinger <a.rueedlinger@gmail.com>
 * @date      12.03.2015
 *
 * Simple program using a soil moisture sensor.
 * 
 */
int senor_pin = 2;        // sensor pin
int green_led = 8;        // green led
int red_led = 7;          // red led

int val = 0;              // analog pin input value
float max_val = 1023.0f;  // max value of the analog input pin 
float percentage = 0;     // soil moisture in percentage
float threshold = 30.0;   // min 30.0%


void setup() {
  pinMode(red_led, OUTPUT);
  pinMode(green_led, OUTPUT);
  Serial.begin(9600);
}

void loop() {
  val = analogRead(sensor_pin);    // read the value from the sensor
  percentage = (1.0 - val/max_val) * 100;
  Serial.println(percentage);
  if(percentage - threshold > 0) {
      digitalWrite(green_led, HIGH);
      digitalWrite(red_led, LOW);
  } else {
      digitalWrite(green_led, LOW);
      digitalWrite(red_led, HIGH);
  }
  delay(200);
}
 

