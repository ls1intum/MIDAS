//
//  MDEncodeTypes.m
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDEncodeTypes.h"

uint8_t kMDEncoded_RecordedGesture = 0x10;
uint8_t kMDEncoded_Gesture = 0x20;
uint8_t kMDEncoded_TouchSequence = 0x30;
uint8_t kMDEncoded_Touch = 0x40;

uint8_t kMDEncoded_Action = 0x60;
uint8_t kMDEncoded_Segue = 0x61;

uint8_t kMDEncoded_Context_Device = 0x80;
uint8_t kMDEncoded_Context_DeviceState = 0x81;
uint8_t kMDEncoded_Context_Time = 0x90;
uint8_t kMDEncoded_Context_Battery = 0xA0;
uint8_t kMDEncoded_Context_Connectivity = 0xB0;
uint8_t kMDEncoded_Context_Motion = 0xC0;
uint8_t kMDEncoded_Context_Activity = 0xC2;
uint8_t kMDEncoded_Context_Location = 0xC1;


