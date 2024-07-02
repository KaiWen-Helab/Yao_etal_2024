// For the licking paradigm on training Day2
//  On Day2, each mouse was required to got watered 80times from the water outlet
//   Unlike on Day1, only within rhe 5-second time window (10~20 seconds after a sound cue),
//   the mouse can be watered if licking the water outlet 


int wateredtrialNumber=0;
int trialNumber=0;
int touch = 13;
int speaker =11;
int frequency = 3000;
int trigger_Inper =8 ;
int pump = 10;
int totalTrial = 0;
int wateredTrial = 0;
int wrongTrial = 0;
int missedTrial = 0;


void setup() {
    pinMode(touch,INPUT);
    pinMode(pump,OUTPUT); 
    pinMode(speaker,OUTPUT);  
    pinMode(trigger_Inper,OUTPUT);  
    digitalWrite(pump,HIGH); /
    digitalWrite(trigger_Inper,LOW); 
    //Set the pins of the singlechip
  Serial.begin(9600);
}

void loop() {
  trialNumber++; 

  tone(speaker,frequency);
  delay(200); 
  noTone(speaker); //The sound lasts for 0.2sec, 3000Hz.
          
  for (int j=0; j <= 250; ){
  int value = digitalRead(touch);  
      if (value==HIGH) {   
                 
        digitalWrite(trigger_Inper,HIGH); 
        delay(30);
        digitalWrite(trigger_Inper,LOW);  
              
        digitalWrite(pump,LOW); 
        delay(100);
        digitalWrite(pump,HIGH); 
        delay(500);
        digitalWrite(pump,LOW); 
        delay(100);
        digitalWrite(pump,HIGH);
        
        delay(5000); 
        
        wateredtrialNumber++;
        j=j+300; //Water given only once.
        }
    j++;
    delay(20);  
  }  //Check whether there was a lick within the 5sec time window. If so, the pump would run.

 int interval = random(10000,20000);  //random time interval bewteen the cue and watering time window
 delay(interval);
 Serial.print("totalTrials:");Serial.print(trialNumber);
 Serial.print(" wateredTrials:");Serial.println(wateredtrialNumber);

 if (wateredtrialNumber==80){
  Serial.println("----------already 80 times----------------");
  while(1){}
  }
  //Stop after 80 watered trails finished.
 
} 
