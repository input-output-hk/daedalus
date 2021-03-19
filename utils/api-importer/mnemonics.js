// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1471
const byronMnemonics = [
  ['arctic', 'decade', 'pink', 'easy', 'jar', 'index', 'base', 'bright', 'vast', 'ocean', 'hard', 'pizza'],
  ['finish', 'evoke', 'alone', 'town', 'express', 'wide', 'pair', 'story', 'west', 'safe', 'news', 'wrap'],
  ['fox', 'now', 'hello', 'inmate', 'era', 'jealous', 'cruel', 'wreck', 'dash', 'supply', 'book', 'attend'],
  ['must', 'lock', 'cereal', 'water', 'silver', 'cake', 'circle', 'express', 'sock', 'arm', 'chapter', 'avoid'],
  ['give', 'verb', 'balcony', 'hurdle', 'pistol', 'flee', 'manage', 'barely', 'pulse', 'episode', 'speak', 'school'],
  ['divert', 'entire', 'urge', 'banner', 'repair', 'mechanic', 'muffin', 'illness', 'genre', 'intact', 'coin', 'boss'],
  ['pink', 'radio', 'various', 'frame', 'argue', 'draft', 'sun', 'speak', 'club', 'salute', 'thank', 'price'],
  ['all', 'beef', 'link', 'funny', 'swing', 'duck', 'sweet', 'swallow', 'slow', 'shield', 'weekend', 'open'],
  ['green', 'friend', 'captain', 'entry', 'utility', 'lake', 'blur', 'matrix', 'will', 'prefer', 'breeze', 'shed'],
  ['reveal', 'jazz', 'equal', 'salmon', 'first', 'decline', 'liquid', 'wolf', 'powder', 'account', 'elbow', 'figure'],
];

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1876
const shelleyMnemonics = [
  ['ketchup', 'embody', 'define', 'thing', 'few', 'tornado', 'worry', 'few', 'wisdom', 'people', 'sure', 'bean', 'ring', 'impact', 'clerk', 'mirror', 'antenna', 'truly', 'chief', 'truth', 'sign', 'drip', 'sorry', 'flush'],
  ['obscure', 'protect', 'still', 'woman', 'rescue', 'plunge', 'lemon', 'warm', 'cash', 'quote', 'wood', 'adapt', 'erase', 'muffin', 'blush', 'diet', 'noodle', 'biology', 'scrap', 'involve', 'radar', 'filter', 'oval' ,'filter'],
  ['bird', 'toilet', 'maid', 'mule', 'mercy', 'album', 'powder', 'misery', 'ozone', 'fragile', 'concert', 'media', 'inhale', 'lonely', 'height', 'box', 'enforce', 'mesh', 'budget', 'arch', 'top', 'tenant', 'spoil', 'drop'],
  ['gadget', 'rate', 'fame', 'nothing', 'onion', 'surround', 'loan', 'panel', 'moment', 'used', 'fruit', 'jacket', 'pretty', 'replace', 'pig', 'stairs', 'guard', 'slab', 'shadow', 'child', 'over', 'win', 'focus', 'glue'],
  ['amount', 'become', 'cousin', 'degree', 'practice', 'garbage', 'fall', 'witness', 'mushroom', 'update', 'this', 'define', 'exile', 'fame', 'paper', 'symptom', 'ride', 'oil', 'plate', 'park', 'broom', 'fine', 'six', 'coast'],
  ['nasty', 'abstract', 'scale', 'idle', 'benefit', 'staff', 'normal', 'auto', 'anchor', 'balance', 'measure', 'action', 'crucial', 'virtual', 'lobster', 'wave', 'caution', 'text', 'obey', 'enact', 'only', 'nature', 'illness', 'gain'],
  ['beyond', 'rare', 'pulse', 'setup', 'story', 'side', 'envelope', 'illness', 'warm', 'doll', 'snake', 'turtle', 'oak', 'host', 'horse', 'where', 'rate', 'quantum', 'notice', 'allow', 'monkey', 'shallow', 'police' ,'code'],
  ['brief', 'asset', 'spell', 'behave', 'real', 'galaxy', 'dad', 'solar', 'animal', 'wisdom', 'imitate', 'arch', 'abuse', 'parade', 'loud', 'mention', 'volcano', 'fall', 'awake', 'course', 'solution', 'super', 'guitar', 'rebel'],
  ['onion', 'secret', 'sphere', 'horror', 'hint', 'engine', 'denial', 'six', 'omit', 'shove', 'quit', 'sibling', 'code', 'shallow', 'square', 'athlete', 'dog', 'bleak', 'cost', 'axis', 'alone', 'nut', 'frozen', 'stumble'],
  ['about', 'magnet', 'nut', 'edit', 'awake', 'matrix', 'bamboo', 'casual', 'diamond', 'joke', 'man', 'crumble', 'staff', 'ten', 'potato', 'laptop', 'off', 'action', 'chuckle', 'medal', 'bread', 'blind', 'peanut', 'horse'],
];

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L963
const maryMnemonics = [
  ['engage', 'retire', 'employ', 'north', 'sustain', 'alert', 'spot', 'grape', 'cake', 'embark', 'alien', 'garment', 'cost', 'inmate', 'barrel', 'panel', 'essence', 'repair', 'vendor', 'gain', 'wear', 'cube', 'pole', 'glow'],
  ['sponsor', 'dose', 'cook', 'divide', 'craft', 'tape', 'myth', 'moral', 'final', 'bread', 'ranch', 'kid', 'side', 'remain', 'cinnamon', 'garlic', 'organ', 'combine', 'police', 'theme', 'bracket', 'alert', 'humor', 'bronze'],
  ['beyond', 'sun', 'wash', 'glove', 'ability', 'market', 'enter', 'noodle', 'network', 'kiwi', 'chase', 'snake', 'light', 'medal', 'radar', 'kingdom', 'shed', 'entry', 'sausage', 'two', 'concert', 'pass', 'silent', 'unveil'],
  ['learn', 'chalk', 'cook', 'interest', 'cruise', 'behave', 'cable', 'barrel', 'sort', 'swear', 'cruel', 'spider', 'slam', 'museum', 'private', 'slush', 'artwork', 'basket', 'promote', 'ritual', 'erode', 'scan', 'book', 'bag'],
  ['multiply', 'one', 'enact', 'relax', 'vehicle', 'already', 'solar', 'ancient', 'work', 'era', 'bunker', 'tuna', 'blossom', 'lottery', 'tackle', 'veteran', 'fish', 'chimney', 'desert', 'common', 'enemy', 'champion', 'guard', 'glory'],
  ['love', 'design', 'option', 'reject', 'suit', 'soon', 'start', 'afford', 'elite', 'riot', 'arrow', 'donate', 'goddess', 'speak', 'punch', 'accident', 'blanket', 'noise', 'cabbage', 'tuition', 'verb', 'chalk', 'magic', 'shuffle'],
  ['cage', 'very', 'sport', 'happy', 'student', 'receive', 'float', 'dance', 'process', 'bus', 'belt', 'supply', 'afford', 'rain', 'hammer', 'guide', 'dust', 'dirt', 'duck', 'north', 'flash', 'tiny', 'torch', 'piano'],
  ['melody', 'mean', 'connect', 'potato', 'salute', 'riot', 'twin', 'damp', 'puzzle', 'depart', 'castle', 'great', 'position', 'child', 'october', 'twist', 'execute', 'fog', 'wasp', 'culture', 'unveil', 'until', 'cousin', 'fee'],
  ['burden', 'drip', 'point', 'mansion', 'asthma', 'spray', 'sort', 'sponsor', 'baby', 'sorry', 'nerve', 'logic', 'crop', 'into', 'rain', 'mouse', 'hint', 'shaft', 'deal', 'endorse', 'way', 'neck', 'ball', 'mango'],
  ['flavor', 'miss', 'awesome', 'purchase', 'crisp', 'flavor', 'fury', 'apology', 'client', 'close', 'mask', 'weather', 'valid', 'reward', 'desert', 'weather', 'weird', 'angle', 'project', 'unit', 'coil', 'blanket', 'web', 'hundred'],
];

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1067
const yoroiMnemonics = [
  ['public', 'wild', 'salad', 'cereal', 'when', 'zone', 'ship', 'circle', 'other', 'second', 'time', 'priority', 'select', 'apart', 'social'],
  ['report', 'weird', 'border', 'gesture', 'since', 'earn', 'motor', 'elbow', 'huge', 'pilot', 'cool', 'civil', 'duty', 'outer', 'exhaust'],
  ['illegal', 'uncover', 'fruit', 'april', 'snap', 'army', 'brown', 'sister', 'situate', 'lunch', 'they', 'fog', 'isolate', 'earn', 'vocal'],
  ['knife', 'satisfy', 'measure', 'around', 'time', 'thought', 'cigar', 'boss', 'truck', 'bar', 'mushroom', 'hold', 'raccoon', 'asset', 'canvas'],
  ['amazing', 'pole', 'kiss', 'expose', 'whip', 'unfair', 'example', 'slice', 'great', 'they', 'element', 'claw', 'photo', 'dwarf', 'green'],
  ['round', 'trend', 'rescue', 'flight', 'awkward', 'enemy', 'luggage', 'range', 'eagle', 'shaft', 'giggle', 'double', 'pencil', 'jazz', 'home'],
  ['talent', 'example', 'renew', 'true', 'amused', 'alcohol', 'immune', 'exclude', 'cat', 'ceiling', 'squeeze', 'cover', 'slender', 'pond', 'turkey'],
  ['box', 'elegant', 'raccoon', 'brick', 'uphold', 'behind', 'blame', 'marble', 'tip', 'move', 'gift', 'juice', 'crystal', 'circle', 'sound'],
  ['mango', 'street', 'flush', 'universe', 'clap', 'system', 'talk', 'steel', 'tray', 'target', 'forum', 'dust', 'brisk', 'expose', 'prevent'],
  ['behind', 'rib', 'say', 'absorb', 'enroll', 'pyramid', 'balance', 'strategy', 'response', 'evolve', 'pipe', 'dolphin', 'shift', 'flag', 'history'],
];

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1832
const itnByronMnemonics = [
  ['phrase', 'rapid', 'fine', 'neglect', 'already', 'nut', 'note', 'chair', 'mushroom', 'rack', 'ivory', 'riot'],
  ['ivory', 'citizen', 'rule', 'scare', 'angle', 'method', 'bounce', 'caution', 'noble', 'pottery', 'plunge', 'resource'],
  ['behave', 'attitude', 'glide', 'else', 'have', 'moon', 'settle', 'minute', 'provide', 'trade', 'negative', 'nothing'],
  ['diary', 'chunk', 'total', 'cruise', 'they', 'curious', 'foil', 'actress', 'wish', 'universe', 'grape', 'kind'],
  ['mushroom', 'print', 'dish', 'slim', 'agent', 'tube', 'expand', 'actor', 'layer', 'idea', 'example', 'quarter'],
  ['riot', 'sport', 'access', 'grid', 'destroy', 'chronic', 'evil', 'doll', 'sibling', 'blanket', 'seed', 'goose'],
  ['pyramid', 'song', 'photo', 'filter', 'subway', 'rich', 'broken', 'anchor', 'blur', 'lecture', 'liar', 'hope'],
  ['sort', 'crouch', 'seven', 'exile', 'extend', 'evoke', 'summer', 'oppose', 'fork', 'result', 'plate', 'goat'],
  ['safe', 'wrap', 'order', 'affair', 'fiber', 'walnut', 'skill', 'timber', 'rookie', 'ghost', 'spot', 'napkin'],
  ['jaguar', 'bitter', 'merry', 'destroy', 'frozen', 'dune', 'embody', 'pull', 'cradle', 'peasant', 'sail', 'whisper'],
];

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L159
const itnShelleyMnemonics = [
  ['vintage', 'poem', 'topic', 'machine', 'hazard', 'cement', 'dune', 'glimpse', 'fix', 'brief', 'account', 'badge', 'mass', 'silly', 'business'],
  ['shift', 'mistake', 'rural', 'security', 'inspire', 'loyal', 'wink', 'special', 'blast', 'retreat', 'crouch', 'noise', 'dirt', 'around', 'drastic'],
  ['soldier', 'this', 'verb', 'copper', 'immune', 'unveil', 'engine', 'know', 'tower', 'only', 'foot', 'riot', 'orchard', 'member', 'guitar'],
  ['cupboard', 'fringe', 'garment', 'dawn', 'caught', 'cream', 'alpha', 'sorry', 'unusual', 'federal', 'item', 'leopard', 'lawn', 'rescue', 'desk'],
  ['glad', 'hold', 'sweet', 'tobacco', 'topple', 'rich', 'grab', 'bridge', 'adjust', 'excess', 'improve', 'job', 'lottery', 'diary', 'dust'],
  ['all', 'flee', 'sugar', 'mail', 'response', 'minimum', 'bulk', 'stone', 'cost', 'dynamic', 'forget', 'embrace', 'spray', 'ocean', 'luggage'],
  ['kiwi', 'million', 'space', 'squirrel', 'deliver', 'galaxy', 'cat', 'private', 'meadow', 'canvas', 'differ', 'rescue', 'artist', 'laptop', 'claim'],
  ['length', 'alpha', 'return', 'angle', 'siren', 'buyer', 'reject', 'absurd', 'piece', 'crash', 'toilet', 'flag', 'viable', 'brick', 'sense'],
  ['viable', 'become', 'talk', 'benefit', 'start', 'shield', 'chapter', 'skull', 'donor', 'hip', 'place', 'aware', 'acquire', 'mango', 'hold'],
  ['awkward', 'electric', 'strong', 'early', 'rose', 'abuse', 'mutual', 'limit', 'ketchup', 'child', 'limb', 'exist', 'hurry', 'business', 'whisper'],
];

module.exports = {
  byronMnemonics,
  shelleyMnemonics,
  maryMnemonics,
  yoroiMnemonics,
  itnByronMnemonics,
  itnShelleyMnemonics,
};
