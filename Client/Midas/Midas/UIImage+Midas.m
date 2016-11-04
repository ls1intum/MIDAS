//
//  UIImage+Midas.m
//  Midas
//
//  Created by Thomas Günzel on 17/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "UIImage+Midas.h"

@implementation UIImage (Midas)

+(NSBundle*)midasBundle {
	static NSBundle *bundle = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		bundle = [NSBundle bundleWithIdentifier:@"de.tum.in.www1.tg.midas"];
	});
	return bundle;
}

+(UIImage *)midasImageNamed:(NSString *)name {
	return [UIImage imageNamed:name inBundle:[self midasBundle] compatibleWithTraitCollection:nil];
}

@end
