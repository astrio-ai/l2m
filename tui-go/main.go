package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// Styles
var (
	// Input box style - with background
	inputBoxStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B4B4B4")).
			Background(lipgloss.Color("#2b2b2b")).
			Padding(0, 1)

	// User input prompt style
	promptStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B4B4B4")).
			Bold(true)

	// Assistant output style
	assistantStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#787878"))

	// Error style
	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B45A5A"))

	// Warning style
	warningStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B4825A"))

	// Success style
	successStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#789678"))

	// Separator style
	separatorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#505050"))
)

type model struct {
	input           string
	cursor          int
	messages        []message
	pythonCmd       *exec.Cmd
	pythonStdin     io.WriteCloser
	pythonStdout    io.ReadCloser
	waiting         bool
	err             error
	terminalHeight  int
	terminalWidth   int
	currentResponse *strings.Builder
	mu              sync.Mutex
}

type message struct {
	role    string // "user", "assistant", "error", "system"
	content string
}

type pythonOutputMsg struct {
	content string
	isError bool
	done    bool
}

type pythonErrorMsg struct {
	err error
}

func initialModel() model {
	return model{
		input:           "",
		cursor:          0,
		messages:        []message{},
		waiting:         false,
		currentResponse: &strings.Builder{},
	}
}

func (m model) Init() tea.Cmd {
	// Start in demo mode for now
	// TODO: Implement proper Python backend integration
	return nil
}

type pythonStartedMsg struct {
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	stdout io.ReadCloser
	err    error
}

// Start the Python L2M backend
func startPythonBackend() tea.Msg {
	// Get the parent directory (go back from tui-go to project root)
	workDir := ".."
	if wd, err := os.Getwd(); err == nil {
		// If we're already in tui-go, go up one level
		if strings.HasSuffix(wd, "tui-go") {
			workDir = ".."
		} else {
			workDir = "."
		}
	}
	
	// Start Python in demo mode (skip TUI backend for now)
	// Just use regular L2M with minimal output
	cmd := exec.Command("python", "-m", "cli.main", "--yes-always")
	cmd.Dir = workDir
	
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return pythonStartedMsg{err: fmt.Errorf("failed to create stdin pipe: %w", err)}
	}
	
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return pythonStartedMsg{err: fmt.Errorf("failed to create stdout pipe: %w", err)}
	}
	
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return pythonStartedMsg{err: fmt.Errorf("failed to create stderr pipe: %w", err)}
	}
	
	// Start the command
	if err := cmd.Start(); err != nil {
		return pythonStartedMsg{err: fmt.Errorf("failed to start Python backend: %w", err)}
	}
	
	// Discard stderr to avoid noise
	go func() {
		io.Copy(io.Discard, stderr)
	}()
	
	// Skip initial startup messages
	go func() {
		scanner := bufio.NewScanner(stdout)
		for scanner.Scan() {
			line := scanner.Text()
			// Skip startup info lines
			if strings.Contains(line, "Using") ||
				strings.Contains(line, "L2M v") ||
				strings.Contains(line, "Main model:") ||
				strings.Contains(line, "Weak model:") ||
				strings.Contains(line, "Git repo:") ||
				strings.Contains(line, "Repo-map:") ||
				strings.Contains(line, "Note:") ||
				strings.Contains(line, "Git working dir:") ||
				strings.Contains(line, "Cur working dir:") ||
				strings.HasPrefix(line, ">") {
				continue // Skip these lines
			}
			// Once we get past startup, we're ready
			break
		}
	}()
	
	return pythonStartedMsg{
		cmd:    cmd,
		stdin:  stdin,
		stdout: stdout,
		err:    nil,
	}
}

// Wait for output from Python
func waitForPythonOutput(stdout io.ReadCloser) tea.Cmd {
	return func() tea.Msg {
		if stdout == nil {
			return pythonOutputMsg{done: true}
		}
		
		scanner := bufio.NewScanner(stdout)
		for scanner.Scan() {
			line := scanner.Text()
			// Return each line as it comes
			return pythonOutputMsg{content: line, done: false}
		}
		
		if err := scanner.Err(); err != nil {
			return pythonErrorMsg{err: err}
		}
		
		return pythonOutputMsg{done: true}
	}
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.terminalHeight = msg.Height
		m.terminalWidth = msg.Width
		return m, nil

	case pythonStartedMsg:
		if msg.err != nil {
			m.err = msg.err
			return m, nil
		}
		// Store the handles
		m.pythonCmd = msg.cmd
		m.pythonStdin = msg.stdin
		m.pythonStdout = msg.stdout
		// Start waiting for output
		return m, waitForPythonOutput(m.pythonStdout)

	case pythonOutputMsg:
		if msg.isError {
			m.messages = append(m.messages, message{
				role:    "error",
				content: msg.content,
			})
			m.waiting = false
			return m, nil
		}
		
		if msg.done {
			// Response complete
			if m.currentResponse.Len() > 0 {
				m.messages = append(m.messages, message{
					role:    "assistant",
					content: m.currentResponse.String(),
				})
				m.currentResponse.Reset()
			}
			m.waiting = false
			return m, nil
		}
		
		// Accumulate response
		m.currentResponse.WriteString(msg.content)
		m.currentResponse.WriteString("\n")
		
		// Continue waiting for more output
		return m, waitForPythonOutput(m.pythonStdout)

	case pythonErrorMsg:
		m.err = msg.err
		m.waiting = false
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "ctrl+d":
			// Cleanup Python process
			if m.pythonCmd != nil && m.pythonCmd.Process != nil {
				m.pythonCmd.Process.Kill()
			}
			return m, tea.Quit

		case "enter":
			if m.input == "" || m.waiting {
				return m, nil
			}
			
			// Add user message
			userInput := strings.TrimSpace(m.input)
			m.messages = append(m.messages, message{
				role:    "user",
				content: userInput,
			})
			
			// Demo mode - simulate response
			m.messages = append(m.messages, message{
				role:    "assistant",
				content: "✨ This is the Go TUI demo mode!\n\n" +
					"The beautiful input box with background is working! 🎨\n\n" +
					"To connect the Python backend, we need to implement a proper\n" +
					"JSON-based protocol. For now, enjoy the clean UI!\n\n" +
					"Your input was: " + userInput,
			})
			
			m.input = ""
			m.cursor = 0
			return m, nil

		case "backspace":
			if m.cursor > 0 {
				m.input = m.input[:m.cursor-1] + m.input[m.cursor:]
				m.cursor--
			}
			return m, nil

		case "left":
			if m.cursor > 0 {
				m.cursor--
			}
			return m, nil

		case "right":
			if m.cursor < len(m.input) {
				m.cursor++
			}
			return m, nil

		case "home", "ctrl+a":
			m.cursor = 0
			return m, nil

		case "end", "ctrl+e":
			m.cursor = len(m.input)
			return m, nil

		default:
			// Regular character input
			if !m.waiting && len(msg.String()) == 1 {
				m.input = m.input[:m.cursor] + msg.String() + m.input[m.cursor:]
				m.cursor++
			}
			return m, nil
		}
	}

	return m, nil
}

func (m model) View() string {
	var output strings.Builder

	// Calculate available height for messages
	availableHeight := m.terminalHeight - 5 // Reserve space for input and help

	// Display messages (show last N messages that fit)
	if len(m.messages) > 0 {
		// Calculate how many messages to show
		displayMessages := m.messages
		if len(displayMessages) > availableHeight/3 {
			// Show most recent messages
			displayMessages = m.messages[len(m.messages)-availableHeight/3:]
		}

		for i, msg := range displayMessages {
			switch msg.role {
			case "user":
				output.WriteString(promptStyle.Render("> "))
				output.WriteString(msg.content)
				
			case "assistant":
				output.WriteString(separatorStyle.Render("▌"))
				output.WriteString("\n\n")
				output.WriteString(assistantStyle.Render(msg.content))
				
			case "error":
				output.WriteString(errorStyle.Render("✗ "))
				output.WriteString(errorStyle.Render(msg.content))
				
			case "system":
				output.WriteString(warningStyle.Render("● "))
				output.WriteString(warningStyle.Render(msg.content))
			}
			
			if i < len(displayMessages)-1 {
				output.WriteString("\n\n")
			}
		}
		output.WriteString("\n\n")
	}

	// Show current response if waiting
	if m.waiting && m.currentResponse.Len() > 0 {
		output.WriteString(separatorStyle.Render("▌"))
		output.WriteString("\n\n")
		output.WriteString(assistantStyle.Render(m.currentResponse.String()))
		output.WriteString(assistantStyle.Render("..."))
		output.WriteString("\n\n")
	}

	// Display error if any
	if m.err != nil {
		output.WriteString(errorStyle.Render(fmt.Sprintf("Error: %v\n\n", m.err)))
	}

	// Display input box with background
	inputPrefix := promptStyle.Render("> ")
	inputText := m.input
	
	// Add cursor
	if m.cursor == len(m.input) {
		inputText += "█" // Cursor at end
	} else if m.cursor < len(m.input) {
		// Insert cursor in the middle
		inputText = m.input[:m.cursor] + "█" + m.input[m.cursor:]
	}
	
	// Apply background to the entire input line
	styledInput := inputBoxStyle.Render(inputPrefix + inputText)
	output.WriteString(styledInput)
	
	// Waiting indicator
	if m.waiting {
		output.WriteString(" ")
		output.WriteString(assistantStyle.Render("●●●"))
	}

	return output.String()
}

func main() {
	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
