//
//  MDRecordedAction.h
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "MDCoding.h"

@interface MDRecordedAction : NSObject<MDCoding>

-(instancetype)initWithViewController:(NSString*)viewController selector:(NSString*)selector senderKey:(NSString*)senderKey;

@property(readonly,nonatomic,strong) NSString *viewController;
@property(readonly,nonatomic,strong) NSString *selector;
@property(readonly,nonatomic,strong) NSString *senderKeyPath;

// Downloaded Only!
@property(readonly,nonatomic,strong) NSDictionary *contexts;

-(NSDictionary*)context:(NSString*)contextName;
@end
