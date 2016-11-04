//
//  MDContextProvider.h
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MDContextProvider : NSObject

+(instancetype)sharedProvider;

-(void)increaseNeeded;
-(void)decreaseNeeded;


// For Subclasses only
-(void)_start;
-(void)_stop;

@end
