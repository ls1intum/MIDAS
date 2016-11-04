//
//  MDDownload.h
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MDDownload : NSObject

+(void)download:(NSURL*)url completionHandler:(void (^)(NSData *rawData))completionHandler;

@end
