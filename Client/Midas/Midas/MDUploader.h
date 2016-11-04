//
//  MDUploader.h
//  Midas
//
//  Created by Thomas Günzel on 06/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MDUploader : NSObject

-(instancetype)init;

-(void)addFileToUpload:(NSString*)file;

@end
