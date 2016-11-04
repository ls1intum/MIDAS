//
//  MDFilterValue.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilter.h"

@interface MDFilterValue : MDFilter

-(instancetype)initWithName:(NSString *)name image:(UIImage*)image acceptedValue:(id)acceptedValue;
+(instancetype)withValue:(id)acceptedValue name:(NSString*)name image:(UIImage*)image;

@property(readwrite,nonatomic,strong) id acceptedValue;

@end
