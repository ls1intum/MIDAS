//
//  MDScreenshotCache.h
//  Midas
//
//  Created by Thomas Günzel on 18/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

@interface MDScreenshotCache : NSObject

+(instancetype)sharedCache;

-(BOOL)hasScreenshot:(NSString*)viewIdentifier;
-(UIImage*)screenshot:(NSString*)viewIdentifier;

-(void)save:(UIView*)view asScreenshot:(NSString*)viewIdentifier;


@end
