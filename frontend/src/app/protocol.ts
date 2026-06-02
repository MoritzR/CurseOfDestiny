export interface GameSnapshot {
  currentPlayer: string;
  nextCardId: number;
  players: PlayerSnapshot[];
}

export interface PlayerSnapshot {
  playerId: string;
  name: string;
  schicksalsmacht: number;
  hand: CardSnapshot[];
  field: CardSnapshot[];
  graveyard: CardSnapshot[];
  deck: CardSnapshot[];
}

export interface CardSnapshot {
  cardId: number;
  owner: string;
  name: string;
  cost: string;
  cardType: string;
  baseStrength: number | null;
  currentStrength: number | null;
  tags: string[];
  description: string;
  modifications: ModificationSnapshot[];
}

export interface ModificationSnapshot {
  kind: string;
  duration: string;
  amount: number | null;
  description: string;
}

export type Prompt =
  | { type: 'CommandPrompt'; payload: { message: string } }
  | { type: 'ChoicePrompt'; payload: { message: string; options: string[] } }
  | { type: 'NumberPrompt'; payload: { message: string } };

export type ServerMessage =
  | { type: 'Connected'; payload: { message: string } }
  | { type: 'StateSnapshot'; payload: { state: GameSnapshot } }
  | { type: 'PromptMessage'; payload: { prompt: Prompt } }
  | { type: 'NoticeMessage'; payload: { message: string } }
  | { type: 'ErrorMessage'; payload: { message: string } };

export type ClientMessage =
  | { type: 'SubmitCommand'; payload: { command: string } }
  | { type: 'SubmitChoice'; payload: { choiceIndex: number } }
  | { type: 'SubmitNumber'; payload: { number: number } };
