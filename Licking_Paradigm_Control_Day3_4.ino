// For the licking paradigm on training Day3 and test Day
//  On Day3, each mouse was required to got watered 60times from the water outlet
//    Watering condition: the mouse needs to hold not to lick for 5s following the sound cue, 
//    then it can get watered if licking the outlet within the subsequent 5-second time window.
//    Any licks during the holding session leads to an extra 5-second delay.
//      Recording of licking actions with timestamps was obtained form the serial monitor of Arduino for further analysis

int touch = 13;
int speaker =11;
int frequency = 3000;
int trigger_Inper =12 ;
int pump = 10;
int totalTrial = 0;
int wateredTrial = 0;
int wrongTrial = 0;
int missedTrial = 0;

void setup()  {
  // put your setup code here, to run once:
  pinMode(touch,INPUT);
  pinMode(pump,OUTPUT); 
  pinMode(speaker,OUTPUT);  
  pinMode(trigger_Inper,OUTPUT);  
  digitalWrite(pump,HIGH); 
  digitalWrite(trigger_Inper,LOW); 
  //Set the pins of the singlechip
  Serial.begin(9600);
  //Set the serial communication
  delay(5000);
}

void loop() {
  // put your main code here, to run repeatedly:
  int lickA = 0;
  int lickB = 0;
  
  for (int k=0; k < 100; ){
    int value = digitalRead(touch);
      if (value==1) {
      lickB++;
      delay(50); 
      k++;
      }
      else{delay(50); k++; }
  }
  
  Serial.println(" ");
  //A blank row indicates the begining of a new trial
  if(lickB == 0){
      totalTrial++; 
      
      tone(speaker,frequency);
      delay(200); 
      noTone(speaker); 
      
      Serial.print(1); Serial.print("  "); Serial.print(0); Serial.print("  "); Serial.println(0);
      //Print "1 0 0", which indicates the sound cue was given
      Serial.print("wateredTrials: ");Serial.println(wateredTrial);
      Serial.print("totalTrials: ");Serial.println(totalTrial);

        for (int j=0; j < 250; ){
        int value = digitalRead(touch);  
            if (value==HIGH) {   
              
              Serial.print(0); Serial.print("  "); Serial.print(1); Serial.print("  "); Serial.println(0);
              //Print "0 1 0", which indicates water pump-out
                       
              digitalWrite(trigger_Inper,HIGH); 
              delay(20);
              digitalWrite(trigger_Inper,LOW);  
              // Trigger event marker on photometry recording software(Inper)
                    
              digitalWrite(pump,LOW); 
              delay(100);
              digitalWrite(pump,HIGH); 
              delay(500);
              digitalWrite(pump,LOW); 
              delay(100);
              digitalWrite(pump,HIGH);

              delay(5000); 

              lickA++;
              wateredTrial++;
              j=j+300; //Water given only once.
            }
        j++;
        delay(20);  
        } //Check whether there was a lick within the 5sec time window. If so, the pump would run.
      if(lickA == 0){ missedTrial++; }
   }

  if(wateredTrial==60){
    Serial.println("----already 60 times---");
    Serial.print("totalTrials: ");Serial.println(totalTrial);
    Serial.print("missedTrials: ");Serial.println(missedTrial);
    wateredTrial++;   
    while(1){} 
    }
    //Stop after 60 watered trails finished.

}
