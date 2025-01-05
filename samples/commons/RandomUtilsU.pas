// *************************************************************************** }
//
// Delphi Fake Data Utils
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphi_fake_data_utils
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
{$DEFINE GENERATE_DATASETS}

unit RandomUtilsU;

interface

{$IF Defined(GENERATE_DATASETS)}
uses
  Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDIF}

const
  FirstNames: array [0 .. 29] of string = (
    'Daniele',
    'Debora',
    'Mattia',
    'Jack',
    'James',
    'William',
    'Joseph',
    'David',
    'Charles',
    'Thomas',
    'Ethan',
    'Liam',
    'Noah',
    'Logan',
    'Lucas',
    'Mason',
    'Benjamin',
    'Alexander',
    'Elijah',
    'Jordan',
    'Alexander',
    'Jamie',
    'Tyler',
    'Caleb',
    'Kieran',
    'Ryan',
    'Colton',
    'Jaxon',
    'Gavin',
    'Ryder'
    );

  LastNames: array [0 .. 13] of string = (
    'Smith',
    'Johnson',
    'Williams',
    'Brown',
    'Black',
    'Red',
    'Green',
    'Willis',
    'Jones',
    'Miller',
    'Davis',
    'Wilson',
    'Martinez',
    'Anderson'
    );

  Countries: array [0 .. 24] of string = (
    'italy',
    'new york',
    'illinois',
    'arizona',
    'nevada',
    'uk',
    'france',
    'georgia',
    'spain',
    'portugal',
    'germany',
    'norway',
    'california',
    'usa',
    'japan',
    'australia',
    'singapore',
    'hong kong',
    'taiwan',
    'south africa',
    'canada',
    'switzerland',
    'sweden',
    'netherlands',
    'belgium'
  );

LOREM_IPSUM =
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.' +
  'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.' +
  'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.' +
  'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';

    WORDS: array [0 .. 1000] of string = ('bite', 'mate', 'quill', 'back', 'church', 'pear', 'knit', 'bent', 'wrench', 'crack', 'heavenly',
      'deceive', 'maddening', 'plain', 'writer', 'rapid', 'acidic', 'decide', 'hat', 'paint', 'cow', 'dysfunctional', 'pet', 'giraffe',
      'connection', 'sour', 'voracious', 'cloudy', 'wry', 'curve', 'agree', 'eggnog', 'flaky', 'painstaking', 'warm', 'silk', 'icy',
      'hellish', 'toy', 'milky', 'skirt', 'test', 'daffy', 'questionable', 'gamy', 'aware', 'berry', 'throne', 'oven', 'subtract', 'cool',
      'care', 'charge', 'smash', 'curve', 'comfortable', 'narrow', 'merciful', 'material', 'fear', 'exercise', 'skinny', 'fire',
      'rainstorm', 'tail', 'nondescript', 'calculating', 'pack', 'steel', 'marvelous', 'baseball', 'furtive', 'stitch', 'abiding', 'empty',
      'bushes', 'painful', 'tense', 'verse', 'unwritten', 'reproduce', 'receptive', 'bottle', 'silky', 'alleged', 'stingy', 'irritate',
      'expand', 'cap', 'unsuitable', 'gigantic', 'exist', 'damp', 'scrub', 'disgusted', 'sun', 'ink', 'detailed', 'defeated', 'economic',
      'chunky', 'stop', 'overflow', 'numerous', 'joyous', 'wipe', 'drink', 'error', 'branch', 'male', 'proud', 'soggy', 'ship', 'excite',
      'industry', 'wistful', 'man', 'vacation', 'doctor', 'naughty', 'plane', 'ignore', 'open', 'act', 'earthquake', 'inconclusive',
      'reflect', 'force', 'funny', 'wonder', 'magenta', 'near', 'dam', 'windy', 'maid', 'wacky', 'release', 'birthday', 'statement',
      'psychotic', 'quicksand', 'things', 'planes', 'boundary', 'nod', 'touch', 'argue', 'sin', 'train', 'adhoc', 'needle', 'regret',
      'stroke', 'strengthen', 'bruise', 'mine', 'rod', 'tax', 'twig', 'advise', 'stamp', 'rhyme', 'obnoxious', 'few', 'inform', 'fixed',
      'mailbox', 'bells', 'grade', 'machine', 'yarn', 'lighten', 'tub', 'guiltless', 'hot', 'misty', 'van', 'flap', 'nosy', 'neighborly',
      'crime', 'nifty', 'uninterested', 'noisy', 'oafish', 'squeal', 'page', 'wet', 'embarrassed', 'long-term', 'closed', 'language',
      'argument', 'elite', 'ban', 'trip', 'tour', 'wine', 'profit', 'envious', 'love', 'back', 'bite-sized', 'magical', 'snatch', 'elated',
      'sniff', 'far', 'shy', 'deeply', 'zoom', 'invent', 'downtown', 'heartbreaking', 'angry', 'can', 'bucket', 'important', 'fetch',
      'shoe', 'self', 'x-ray', 'abhorrent', 'lumpy', 'fertile', 'nest', 'pick', 'history', 'offbeat', 'interrupt', 'yell', 'grain',
      'scintillating', 'alluring', 'wren', 'form', 'attack', 'foregoing', 'suspect', 'daughter', 'moldy', 'signal', 'placid', 'quirky',
      'itchy', 'butter', 'ordinary', 'imaginary', 'list', 'known', 'servant', 'slow', 'apparel', 'meeting', 'lovely', 'bat', 'insurance',
      'waste', 'aromatic', 'foot', 'breakable', 'theory', 'stiff', 'cream', 'train', 'ground', 'fuel', 'wary', 'store', 'wonderful', 'corn',
      'zippy', 'dashing', 'risk', 'prose', 'try', 'green', 'bead', 'recess', 'chop', 'stain', 'faded', 'heat', 'camera', 'panicky',
      'depressed', 'wooden', 'clumsy', 'gullible', 'railway', 'guide', 'current', 'giants', 'enter', 'talented', 'bustling', 'square',
      'jewel', 'bee', 'jelly', 'utopian', 'heal', 'anger', 'balance', 'tick', 'turn', 'unique', 'lively', 'wrist', 'fade', 'tender',
      'outgoing', 'own', 'sigh', 'jobless', 'boiling', 'parallel', 'vest', 'leather', 'spark', 'suck', 'knot', 'circle', 'square', 'supply',
      'tank', 'fax', 'spotless', 'habitual', 'feeling', 'watch', 'cattle', 'end', 'true', 'zonked', 'poison', 'man', 'pedal', 'boorish',
      'moaning', 'mindless', 'bone', 'spot', 'chubby', 'numberless', 'eye', 'bright', 'sweet', 'fanatical', 'oranges', 'calm', 'squash',
      'tooth', 'petite', 'design', 'one', 'bump', 'aberrant', 'mine', 'fit', 'rub', 'optimal', 'ugly', 'lyrical', 'borrow', 'queue',
      'alert', 'normal', 'wrathful', 'truculent', 'level', 'hollow', 'disillusioned', 'kick', 'weather', 'mighty', 'upbeat', 'troubled',
      'snotty', 'many', 'warn', 'thank', 'trains', 'plan', 'choke', 'activity', 'attend', 'walk', 'thought', 'gabby', 'actor', 'prickly',
      'smell', 'dangerous', 'observation', 'action', 'steady', 'hypnotic', 'second-hand', 'zip', 'mundane', 'sand', 'sneaky', 'harm',
      'pancake', 'guarantee', 'empty', 'bulb', 'burn', 'reject', 'decorate', 'obese', 'crowd', 'clap', 'flat', 'available', 'hop', 'untidy',
      'wreck', 'fasten', 'waves', 'dinosaurs', 'dreary', 'fearful', 'answer', 'parched', 'tight', 'animated', 'desk', 'jaded', 'wax',
      'silver', 'scream', 'puzzling', 'unbiased', 'unite', 'branch', 'quack', 'writing', 'tease', 'mint', 'full', 'plate', 'gusty', 'bear',
      'bell', 'sparkling', 'absurd', 'past', 'earsplitting', 'seemly', 'unadvised', 'paper', 'battle', 'friend', 'control', 'rich',
      'regret', 'used', 'scattered', 'redundant', 'slave', 'languid', 'didactic', 'fairies', 'sofa', 'spiteful', 'reply', 'division',
      'engine', 'suppose', 'homeless', 'pinch', 'ray', 'channel', 'repeat', 'smoke', 'concentrate', 'handy', 'committee', 'songs', 'madly',
      'itch', 'hands', 'clean', 'addition', 'majestic', 'careful', 'fallacious', 'guarded', 'last', 'time', 'tumble', 'plastic', 'force',
      'guess', 'grape', 'loving', 'hand', 'remain', 'vigorous', 'wash', 'cars', 'same', 'provide', 'shelf', 'yam', 'onerous', 'claim',
      'tramp', 'glistening', 'innocent', 'lock', 'close', 'absorbing', 'daily', 'amuck', 'manage', 'energetic', 'absent', 'fantastic',
      'flippant', 'unnatural', 'amount', 'luxuriant', 'clover', 'alert', 'wheel', 'cellar', 'agonizing', 'card', 'memorise', 'meal',
      'suspend', 'concerned', 'uneven', 'deranged', 'spiritual', 'arch', 'dare', 'hammer', 'tug', 'jump', 'vase', 'plant', 'color', 'worm',
      'grab', 'frame', 'taste', 'incandescent', 'little', 'rule', 'confused', 'roomy', 'gorgeous', 'heat', 'whole', 'cracker', 'water',
      'flimsy', 'high-pitched', 'grandfather', 'spooky', 'natural', 'grease', 'noiseless', 'superficial', 'gaze', 'finger', 'afford',
      'racial', 'tiresome', 'tremendous', 'zealous', 'slip', 'position', 'mountainous', 'shelter', 'calculator', 'tacky', 'whip',
      'mountain', 'clear', 'thin', 'smell', 'ants', 'yellow', 'cross', 'employ', 'trouble', 'dazzling', 'enchanting', 'groovy', 'measure',
      'disapprove', 'elastic', 'sparkle', 'cub', 'foolish', 'discussion', 'stormy', 'pies', 'absorbed', 'trashy', 'mammoth', 'low',
      'subdued', 'badge', 'letter', 'previous', 'challenge', 'tart', 'cute', 'suit', 'condition', 'pricey', 'rule', 'wrong', 'bomb', 'wiry',
      'swim', 'crack', 'disgusting', 'gather', 'half', 'sturdy', 'probable', 'stream', 'trick', 'silly', 'sulky', 'nail', 'rotten', 'stir',
      'sneeze', 'even', 'adamant', 'cluttered', 'object', 'battle', 'petite', 'wait', 'instinctive', 'donkey', 'squeamish', 'rainy',
      'craven', 'acceptable', 'husky', 'pollution', 'judicious', 'distribution', 'neck', 'left', 'collect', 'thankful', 'describe',
      'complex', 'transport', 'horses', 'hope', 'chemical', 'dress', 'idea', 'extend', 'laugh', 'event', 'route', 'hose', 'abundant',
      'insect', 'spectacular', 'whistle', 'home', 'vast', 'massive', 'grey', 'sail', 'lavish', 'word', 'coach', 'repair', 'squeak',
      'curious', 'beam', 'middle', 'obscene', 'efficacious', 'supreme', 'torpid', 'jazzy', 'linen', 'cause', 'synonymous', 'book', 'brave',
      'staking', 'weak', 'show', 'birds', 'barbarous', 'hilarious', 'injure', 'walk', 'screeching', 'frequent', 'wide', 'kiss', 'lonely',
      'quarrelsome', 'arm', 'flowers', 'surround', 'level', 'enjoy', 'calculate', 'reach', 'brother', 'grandiose', 'clammy', 'thunder',
      'pen', 'rake', 'whirl', 'sharp', 'fence', 'scissors', 'polish', 'recondite', 'brief', 'pig', 'ten', 'spell', 'coal', 'sidewalk',
      'straight', 'melted', 'ring', 'deadpan', 'nine', 'wound', 'use', 'switch', 'watch', 'meat', 'governor', 'lively', 'neat', 'dapper',
      'gate', 'rose', 'wealthy', 'psychedelic', 'slap', 'note', 'request', 'match', 'abashed', 'snail', 'tray', 'pump', 'disappear',
      'vegetable', 'wool', 'abstracted', 'impulse', 'fork', 'brake', 'shiny', 'team', 'coherent', 'dust', 'relieved', 'long', 'broad',
      'shop', 'innate', 'milk', 'mother', 'screw', 'cushion', 'listen', 'spot', 'willing', 'legs', 'clever', 'obsolete', 'coil', 'smoke',
      'call', 'men', 'purpose', 'bumpy', 'receipt', 'soothe', 'thinkable', 'launch', 'kittens', 'oceanic', 'dolls', 'jagged', 'fine',
      'start', 'muddled', 'want', 'develop', 'skillful', 'real', 'sisters', 'cooperative', 'retire', 'scarecrow', 'caring', 'chance',
      'search', 'visitor', 'stem', 'rabid', 'seed', 'endurable', 'cloistered', 'knife', 'cast', 'trouble', 'cold', 'brainy', 'admit',
      'base', 'multiply', 'escape', 'bike', 'frighten', 'large', 'pull', 'observant', 'stereotyped', 'dirty', 'tin', 'vague', 'celery',
      'hungry', 'best', 'difficult', 'burly', 'horse', 'flawless', 'fresh', 'inquisitive', 'illegal', 'omniscient', 'simplistic', 'selfish',
      'clean', 'hospital', 'encouraging', 'incompetent', 'right', 'learn', 'relation', 'spoil', 'amused', 'ruthless', 'squalid',
      'aftermath', 'increase', 'greasy', 'futuristic', 'shut', 'friendly', 'steep', 'range', 'faint', 'jail', 'wide-eyed', 'uptight',
      'erratic', 'eyes', 'cure', 'overwrought', 'muddle', 'bedroom', 'scale', 'rub', 'conscious', 'snake', 'box', 'command', 'slippery',
      'handsome', 'spy', 'tongue', 'unbecoming', 'magnificent', 'gold', 'resolute', 'face', 'childlike', 'approval', 'meaty', 'frog',
      'abrasive', 'rat', 'peel', 'office', 'panoramic', 'explode', 'selective', 'ahead', 'thaw', 'mean', 'odd', 'hate', 'window', 'somber',
      'guard', 'riddle', 'judge', 'flock', 'black', 'amusement', 'bikes', 'milk', 'sock', 'historical', 'tawdry', 'bare', 'mitten', 'harsh',
      'street', 'unequal', 'five', 'zinc', 'faulty', 'messy', 'thoughtful', 'spicy', 'oval', 'telephone', 'decisive', 'teeny', 'fix',
      'outstanding', 'excuse', 'abject', 'print', 'receive', 'jump', 'knock', 'ubiquitous', 'anxious', 'fill', 'shrug', 'ossified',
      'penitent', 'dry', 'abaft', 'uncle', 'voiceless', 'spray', 'town', 'aspiring', 'testy', 'bed', 'likeable', 'breezy', 'jumpy', 'talk',
      'powerful', 'various', 'crawl', 'lacking', 'lethal', 'baby', 'sore', 'mourn', 'behave', 'pass', 'mark', 'summer', 'cause',
      'destruction', 'stale', 'basin', 'embarrass', 'rob', 'income', 'overjoyed', 'aback', 'spark', 'air', 'worthless', 'hospitable',
      'dynamic', 'push', 'nervous', 'dark', 'chin', 'shock', 'frame', 'dojo');

function GetRndFirstName: String;
function GetRndLastName: String;
function GetRndFullName: String;
function GetRndCountry: String;
function GetRndEMailAddress: String;
function GetRndDate(const InitialYear: Word = 1980; YearsSpan: Word = 40): TDate;
function GetRndInteger(const aFrom: Integer = 0; aTo: Integer = 1000): Integer;
function GetRndWord: String;
function GetRndPhrase(const aFrom: Integer = 0; aTo: Integer = 1000): String;

{$IF Defined(GENERATE_DATASETS)}
function GetPeople(const Count: Integer = 20): TDataSet;
function GetUsers(const Count: Integer = 10): TDataSet;
function GetPosts(const Count: Integer = 10): TDataSet;
{$ENDIF}

implementation


uses
  System.SysUtils, System.DateUtils, System.Math;

const
  OneDay = OneHour * 24;

function GetRndDate(const InitialYear: Word; YearsSpan: Word): TDate;
begin
  Result := EncodeDate(InitialYear + Random(YearsSpan),1,1) + (OneDay * Random(365));
end;

function GetRndEMailAddress: String;
begin
  Result := GetRndFirstName.Substring(0, RandomRange(1,3)) + '.' +
    GetRndLastName + '@' + GetRndCountry + GetRndInteger(1,3).ToString + '.com';
  Result := Result.Replace(' ', '_', [rfReplaceAll]);
end;

function GetRndCountry: String;
begin
  Result := Countries[Random(High(Countries)+1)];
end;

function GetRndFirstName: String;
begin
  Result := FirstNames[Random(High(FirstNames)+1)];
end;

function GetRndFullName: String;
begin
  Result := GetRndFirstName + ' ' + GetRndLastName;
end;

function GetRndLastName: String;
begin
  Result := LastNames[Random(High(LastNames)+1)];
end;

function GetRndWord: String;
begin
  Result := WORDS[RandomRange(0, Length(WORDS))];
end;

function GetRndPhrase(const aFrom: Integer = 0; aTo: Integer = 1000): String;
var
  WordCount: Integer;
  I: Integer;
begin
  Result := '';
  WordCount := RandomRange(aFrom, aTo);
  for I := 1 to WordCount do
  begin
    Result := Result + GetRndWord + ' ';
  end;
  Result := Result.Trim;
  Result := UpCase(Result.Chars[0]) + Result.Substring(1) + '.';
end;

function GetRndInteger(const aFrom: Integer; aTo: Integer): Integer;
begin
  if aFrom >= aTo then
  begin
    raise Exception.Create('FROM cannot be greater nor equal to TO');
  end;
  Result := RandomRange(aFrom, aTo);
end;


{$IF Defined(GENERATE_DATASETS)}
function GetPeople(const Count: Integer): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('code', ftInteger);
    lMT.FieldDefs.Add('first_name', ftString, 20);
    lMT.FieldDefs.Add('last_name', ftString, 20);
    lMT.FieldDefs.Add('country', ftString, 20);
    lMT.FieldDefs.Add('dob', ftDate);
    lMT.Active := True;
    for I := 1 to Count do
    begin
      lMT.AppendRecord([I, GetRndFirstName, GetRndLastName, GetRndCountry, GetRndDate]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetUsers(const Count: Integer): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('first_name', ftString, 100);
    lMT.FieldDefs.Add('last_name', ftString, 100);
    lMT.FieldDefs.Add('email', ftString, 100);
    lMT.Active := True;
    for I := 1 to Count do
    begin
      lMT.AppendRecord([GetRndFirstName, GetRndLastName, GetRndEMailAddress]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetPosts(const Count: Integer): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('title', ftString, 100);
    lMT.FieldDefs.Add('abstract', ftString, 400);
    lMT.FieldDefs.Add('word_count', ftInteger);
    lMT.FieldDefs.Add('comments', ftInteger);
    lMT.FieldDefs.Add('post_date', ftDate);
    lMT.Active := True;
    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        GetRndPhrase(3, 8),
        GetRndPhrase(30, 50),
        GetRndInteger(20, 5000),
        GetRndInteger(0,20),
        GetRndDate(2020, 4)
        ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;


{$ENDIF}

initialization

Randomize;

end.

