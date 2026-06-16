import { CommonModule } from '@angular/common';
import { Component, computed, inject } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { GameClientService } from './game-client.service';
import { CardSnapshot, PlayerSnapshot, Prompt } from './protocol';

@Component({
  selector: 'cod-root',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  readonly game = inject(GameClientService);
  command = '';
  numberValue = 0;

  readonly players = computed(() => this.game.snapshot()?.players ?? []);
  readonly currentPlayer = computed(() => this.game.snapshot()?.currentPlayer ?? '');

  constructor() {
    this.game.connect();
  }

  submitCommand(): void {
    const command = this.command.trim();
    if (!command) {
      return;
    }

    this.game.submitCommand(command);
    this.command = '';
  }

  submitNumber(): void {
    this.game.submitNumber(this.numberValue);
  }

  isCommandPrompt(prompt: Prompt): prompt is Extract<Prompt, { tag: 'CommandPrompt' }> {
    return prompt.tag === 'CommandPrompt';
  }

  isChoicePrompt(prompt: Prompt): prompt is Extract<Prompt, { tag: 'ChoicePrompt' }> {
    return prompt.tag === 'ChoicePrompt';
  }

  isNumberPrompt(prompt: Prompt): prompt is Extract<Prompt, { tag: 'NumberPrompt' }> {
    return prompt.tag === 'NumberPrompt';
  }

  trackPlayer(_: number, player: PlayerSnapshot): string {
    return player.playerId;
  }

  trackCard(_: number, card: CardSnapshot): number {
    return card.cardId;
  }
}
