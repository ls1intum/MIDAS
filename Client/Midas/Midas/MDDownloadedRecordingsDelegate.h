//
//  MDDownloadedRecordingsDelegate.h
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol MDDownloadedRecordingsDelegate <NSObject>

-(void)recordings:(NSArray*)allRecordings didDownload:(id)downloader;

@end
