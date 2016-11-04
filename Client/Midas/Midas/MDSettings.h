//
//  MDSettings.h
//  Midas
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MDSettings : NSObject

-(instancetype)initWithDictionary:(NSDictionary*)dictionary;
+(instancetype)settingsFromBundledPlist;


@property(readonly,nonatomic,strong) NSString *uploadHost;
@property(readonly,nonatomic) NSUInteger uploadPort;

@property(readonly,nonatomic,strong) NSURL *downloadBaseURL;

@end
