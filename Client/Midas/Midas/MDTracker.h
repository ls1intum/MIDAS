//
//  MDTracker.h
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

@interface MDTracker : NSObject

+(instancetype)sharedTracker;
-(instancetype)initWithDatabaseURL:(NSURL *)url;

-(void)trackAction:(NSString*)selector onViewController:(NSString*)onViewController sender:(NSString*)senderKeyPath;
-(void)trackSegue:(UIStoryboardSegue*)segue sender:(NSString*)sender;

-(void)logEvent:(UIEvent*)event fromWindow:(UIWindow*)window;

/**
 @brief tell the tracker that now would be a good time to upload the recorded data
 */
-(void)adviseUpload;
-(void)endSession;
-(void)startSession;

+(NSURL*)standardDatabaseURL;

@property(readwrite,nonatomic,strong) NSString *currentViewController;
@property(readwrite,nonatomic) BOOL enabled;
@end
