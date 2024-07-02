// For the licking paradigm training Day1
// On Day1, each mouse was required to got watered 100times from the water outlet
// The outlet connect a touch sensor that can send signal to singlechip(Arduino Uno) once the mouse's tongue touched the outlet


int watering = 0; 
int touch = 13;
int speaker =11;
int frequency = 1000; 
int pump = 10;

void setup() {
    pinMode(touch,INPUT);
    pinMode(pump,OUTPUT); 
    pinMode(speaker,OUTPUT);   
    digitalWrite(pump,HIGH); //Set the pins of the singlechip
  Serial.begin(9600);
}

void loop() {
      
          int value = digitalRead(touch);
          if (value==1) {
            // Once the mouse licks, the pump would run, making water is pumped from the outlet
                digitalWrite(pump,LOW); 
                delay(100);
                digitalWrite(pump,HIGH); 
                delay(500);
                digitalWrite(pump,LOW); 
                delay(100);
                digitalWrite(pump,HIGH);
                delay(3000); //Give the mouse 3 sec to consume the water
                watering++; 
          }
          delay(50);  //

       Serial.print ("watering times:");
       Serial.println (watering);

       if( watering==100 ){
        Serial.println ("--------------100 times watered--------------");
        while(1){}
        }
} 
