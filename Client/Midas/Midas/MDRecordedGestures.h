//
//  MDRecordedGestures.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

@class MDEncoder;

@interface MDRecordedGestures : NSObject

-(instancetype)initFromDictionary:(NSDictionary*)dict;

-(instancetype)initWithViewControllerName:(NSString*)viewControllerName encoder:(MDEncoder*)encoder;

-(void)logEvent:(UIEvent *)event;

@property(readonly,nonatomic,strong) NSString *viewControllerName;
@property(readonly,nonatomic,strong) MDEncoder *encoder;


// Downloaded Only!
@property(readonly,nonatomic,strong) NSArray *gestures;
@property(readonly,nonatomic,strong) NSDictionary *contexts;

-(NSDictionary*)context:(NSString*)contextName;
@end
