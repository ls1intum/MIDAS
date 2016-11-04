//
//  MDViewController.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import Foundation;

@class MDTracker;

@protocol MDViewController <NSObject>

-(NSString*)md_identifier;
-(BOOL)md_excludeTitleFromIdentifier;

-(MDTracker*)md_tracker;

-(BOOL)md_trackViewController;

@end
