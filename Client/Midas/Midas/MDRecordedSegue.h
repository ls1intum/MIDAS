//
//  MDRecordedSegue.h
//  Midas
//
//  Created by Thomas Günzel on 18/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "MDCoding.h"

@interface MDRecordedSegue : NSObject<MDCoding>

-(instancetype)initWithSegueName:(NSString*)segueName sourceViewController:(NSString*)source destinationViewController:(NSString*)destinationViewController senderKey:(NSString*)senderKey;

@property(readonly,nonatomic,strong) NSString *sourceViewController;
@property(readonly,nonatomic,strong) NSString *destinationViewController;
@property(readonly,nonatomic,strong) NSString *segueName;
@property(readonly,nonatomic,strong) NSString *senderKey;


// Downloaded Only!
@property(readonly,nonatomic,strong) NSDictionary *contexts;
-(NSDictionary *)context:(NSString *)contextName;

@end
